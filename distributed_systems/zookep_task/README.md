# ZooKeeper Watcher App

# 1. Inicjalizacja katalogow danych klastra
```bash
mkdir -p /tmp/zookeeper/iad /tmp/zookeeper/dub /tmp/zookeeper/pdx
echo 1 > /tmp/zookeeper/iad/myid
echo 2 > /tmp/zookeeper/dub/myid
echo 3 > /tmp/zookeeper/pdx/myid
```

# 2. Start klastra ZooKeeper (kazdy serwer w osobnym terminalu)

**Terminal 1 – serwer iad (port 2181)**
```bash
ZOOCFGDIR=$(pwd)/apache-zookeeper-3.8.4-bin/conf/iad apache-zookeeper-3.8.4-bin/bin/zkServer.sh start-foreground
```

**Terminal 2 – serwer dub (port 2182)**
```bash
ZOOCFGDIR=$(pwd)/apache-zookeeper-3.8.4-bin/conf/dub apache-zookeeper-3.8.4-bin/bin/zkServer.sh start-foreground
```

**Terminal 3 – serwer pdx (port 2183)**
```bash
ZOOCFGDIR=$(pwd)/apache-zookeeper-3.8.4-bin/conf/pdx apache-zookeeper-3.8.4-bin/bin/zkServer.sh start-foreground
```

# 3. Uruchomienie aplikacji (Terminal 4)
```bash
uv run main.py --app "open -a Calculator"
```

# 4. Testowanie (Terminal 5 – zkCli)
```bash
apache-zookeeper-3.8.4-bin/bin/zkCli.sh -server localhost:2181
```
```
create /a ""
create /a/child1 ""
create /a/child2 ""
delete /a/child1
delete /a
```

# 5. Stop klastra
Ctrl+C w terminalach 1, 2, 3.
