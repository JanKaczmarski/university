import argparse
import re
import subprocess
import sys
import threading
from typing import Optional

from kazoo.client import KazooClient
from kazoo.exceptions import NoNodeError
from kazoo.protocol.states import EventType, KazooState


def show_children_count(count: int) -> None:
    msg = f"Aktualna liczba potomkow /a: {count}"
    subprocess.Popen(
        [
            "osascript",
            "-e",
            f'display dialog "{msg}" buttons {{"OK"}} default button "OK" with title "ZooKeeper Watcher"',
        ],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )


_ext_process: Optional[subprocess.Popen] = None
_ext_app_cmd: str = ""
_proc_lock = threading.Lock()

_OPEN_A_RE = re.compile(r'open\s+-a\s+"?([^"]+?)"?\s*$')


def _macos_app_name(cmd: str) -> Optional[str]:
    m = _OPEN_A_RE.match(cmd.strip())
    return m.group(1) if m else None


def start_external_app(app_cmd: str) -> None:
    global _ext_process, _ext_app_cmd
    with _proc_lock:
        if _ext_app_cmd:
            print("[INFO] External app already launched, skipping.")
            return
        print(f"[INFO] Starting external app: {app_cmd}")
        try:
            _ext_process = subprocess.Popen(
                app_cmd,
                shell=True,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            )
            _ext_app_cmd = app_cmd
            print(f"[INFO] External app started (pid={_ext_process.pid})")
        except Exception as exc:
            print(f"[ERROR] Could not start external app: {exc}", file=sys.stderr)


def stop_external_app() -> None:
    global _ext_process, _ext_app_cmd
    with _proc_lock:
        if not _ext_app_cmd:
            return
        app_name = _macos_app_name(_ext_app_cmd) if sys.platform == "darwin" else None
        if app_name:
            print(f"[INFO] Quitting {app_name} via osascript.")
            subprocess.run(
                ["osascript", "-e", f'quit app "{app_name}"'],
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            )
        elif _ext_process is not None and _ext_process.poll() is None:
            print(f"[INFO] Stopping external app (pid={_ext_process.pid})")
            _ext_process.terminate()
            try:
                _ext_process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                _ext_process.kill()
        _ext_process = None
        _ext_app_cmd = ""


# Zookeeper watcher
ZNODE = "/a"


class ZooWatcher:
    def __init__(self, hosts: str, app_cmd: str) -> None:
        self.app_cmd = app_cmd
        self.zk = KazooClient(hosts=hosts)
        self.zk.add_listener(self._state_listener)

    # -- lifecycle

    def start(self) -> None:
        print("[INFO] Connecting to ZooKeeper ...")
        self.zk.start()
        print("[INFO] Connected.")

    def stop(self) -> None:
        self.zk.stop()
        self.zk.close()

    # -- connection state

    def _state_listener(self, state: KazooState) -> None:
        if state == KazooState.LOST:
            print("[WARN] Session lost - stopping external app.")
            stop_external_app()
        elif state == KazooState.SUSPENDED:
            print("[WARN] Connection suspended.")
        elif state == KazooState.CONNECTED:
            print("[INFO] (Re)connected to ZooKeeper.")
            # Must NOT call ZK methods directly from state listener thread
            threading.Thread(target=self._register_exist_watch, daemon=True).start()

    # -- watches

    def _register_exist_watch(self) -> None:
        self.zk.exists(ZNODE, watch=self._on_node_event)
        try:
            total = self._count_descendants(ZNODE)
            self._watch_subtree(ZNODE)
            print(f"[INFO] /a exists - {total} total descendants.")
        except NoNodeError:
            print("[INFO] /a does not exist yet - watching for creation.")

    def _count_descendants(self, path: str) -> int:
        try:
            children = self.zk.get_children(path)
        except NoNodeError:
            return 0
        count = len(children)
        for child in children:
            count += self._count_descendants(f"{path}/{child}")
        return count

    def _watch_subtree(self, path: str) -> None:
        try:
            children = self.zk.get_children(path, watch=self._make_children_watcher(path))
            for child in children:
                self._watch_subtree(f"{path}/{child}")
        except NoNodeError:
            pass

    def _make_children_watcher(self, path: str):

        def _on_children(event):
            if event.type in (EventType.CHILD, EventType.CHANGED):
                try:
                    children = self.zk.get_children(path, watch=_on_children)
                    # Register watches on any newly appeared children
                    for child in children:
                        self._watch_subtree(f"{path}/{child}")
                    total = self._count_descendants(ZNODE)
                    print(f"[EVENT] Subtree of {ZNODE} changed - total descendants: {total}")
                    show_children_count(total)
                except NoNodeError:
                    pass
            elif event.type == EventType.DELETED:
                pass
            else:
                try:
                    self.zk.get_children(path, watch=_on_children)
                except NoNodeError:
                    pass

        return _on_children

    def _on_node_event(self, event) -> None:
        if event.type == EventType.CREATED:
            print(f"[EVENT] {ZNODE} created.")
            start_external_app(self.app_cmd)
            try:
                self._watch_subtree(ZNODE)
                total = self._count_descendants(ZNODE)
                show_children_count(total)
            except NoNodeError:
                pass
            self.zk.exists(ZNODE, watch=self._on_node_event)

        elif event.type == EventType.DELETED:
            print(f"[EVENT] {ZNODE} deleted.")
            stop_external_app()
            self.zk.exists(ZNODE, watch=self._on_node_event)

        else:
            self.zk.exists(ZNODE, watch=self._on_node_event)

    # -- tree printing

    def print_tree(self) -> None:
        """Print the full subtree rooted at /a."""
        try:
            self._print_node(ZNODE, indent=0)
        except NoNodeError:
            print(f"[INFO] Node {ZNODE} does not exist.")

    def _print_node(self, path: str, indent: int) -> None:
        prefix = "    " * indent + ("" if indent == 0 else "├── ")
        data, stat = self.zk.get(path)
        value = data.decode("utf-8", errors="replace") if data else ""
        label = path.split("/")[-1] or path
        print(f"{prefix}{label}  [{value}]  (version={stat.version})")
        try:
            children = self.zk.get_children(path)
        except NoNodeError:
            return
        for child in sorted(children):
            self._print_node(f"{path}/{child}", indent + 1)


def build_arg_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(description="ZooKeeper watcher - monitors /a and manages an external app.")
    p.add_argument(
        "--zookeeper",
        default="localhost:2181,localhost:2182,localhost:2183",
        help="Comma-separated list of host:port (default: localhost:2181,2182,2183)",
    )
    p.add_argument(
        "--app",
        default="open -a Calculator",
        help='External graphical app to launch when /a is created (default: "open -a Calculator")',
    )
    return p


def interactive_loop(watcher: ZooWatcher) -> None:
    print("\nKomendy: 'tree' - wyswietl drzewo /a | 'quit' - zakoncz\n")
    while True:
        try:
            cmd = input("> ").strip().lower()
        except (EOFError, KeyboardInterrupt):
            break
        if cmd in ("quit", "exit", "q"):
            break
        if cmd == "tree":
            watcher.print_tree()
        elif cmd == "":
            pass
        else:
            print("Nieznana komenda. Dostepne: tree, quit")


def main() -> None:
    args = build_arg_parser().parse_args()
    watcher = ZooWatcher(hosts=args.zookeeper, app_cmd=args.app)
    try:
        watcher.start()
        interactive_loop(watcher)
    finally:
        print("[INFO] Shutting down ...")
        stop_external_app()
        watcher.stop()
        print("[INFO] Done.")


if __name__ == "__main__":
    main()
