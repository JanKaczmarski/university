#include <Ice/Ice.h>
#include <Counter.h>

#include <algorithm>
#include <cctype>
#include <cstdlib>
#include <iostream>
#include <memory>
#include <sstream>
#include <string>

namespace {

void printMenu() {
    std::cout << "\n========== I2 Counter Client ==========\n"
              << "Active proxy: ";
}

std::string readLine(const std::string& prompt) {
    std::cout << prompt;
    std::string line;
    std::getline(std::cin, line);
    return line;
}

std::string makeProxyString(const std::string& category, const std::string& name) {
    const char* host_env = std::getenv("ICE_HOST");
    const std::string host = (host_env && *host_env) ? host_env : "localhost";
    std::ostringstream os;
    os << category << "/" << name << ":tcp -h " << host << " -p 10000";
    return os.str();
}

void printOps() {
    std::cout << "  Operations:\n"
              << "    [s] select proxy (category + name)\n"
              << "    [c] checkedCast on the current base proxy\n"
              << "    [u] uncheckedCast on the current base proxy\n"
              << "    [g] getValue\n"
              << "    [v] setValue\n"
              << "    [i] increment\n"
              << "    [r] reset\n"
              << "    [n] getName\n"
              << "    [q] quit\n";
}

}


int main(int argc, char* argv[]) {
    int status = 0;
    try {
        Ice::CommunicatorHolder ich(argc, argv);
        auto communicator = ich.communicator();

        Ice::ObjectPrx base;
        Demo::CounterPrx counter;
        std::string activeDesc = "<none>";

        while (true) {
            printMenu();
            std::cout << activeDesc << "\n";
            printOps();
            std::string cmd = readLine("> ");
            if (cmd.empty()) continue;

            char op = cmd[0];
            try {
                switch (op) {
                case 's': {
                    std::string category = readLine("  category (dedicated/shared): ");
                    std::string name     = readLine("  name (e.g. Alice): ");
                    auto strip = [](std::string& v) {
                        v.erase(std::remove_if(v.begin(), v.end(),
                                    [](unsigned char c){ return std::isspace(c); }),
                                v.end());
                    };
                    strip(category);
                    strip(name);
                    if (category.empty() || name.empty()) {
                        std::cout << "  [WARN] empty input (no spaces allowed)\n";
                        break;
                    }
                    if (category != "dedicated" && category != "shared") {
                        std::cout << "  [WARN] category must be 'dedicated' or 'shared'\n";
                        break;
                    }
                    std::string s = makeProxyString(category, name);
                    base = communicator->stringToProxy(s);
                    counter = nullptr;
                    activeDesc = s + "  (no cast yet)";
                    std::cout << "  base proxy set: " << s << "\n";
                    break;
                }
                case 'c': {
                    if (!base) { std::cout << "  [WARN] select a proxy first\n"; break; }
                    counter = Ice::checkedCast<Demo::CounterPrx>(base);
                    if (!counter) {
                        std::cout << "  [ERROR] checkedCast failed (object does not support Counter)\n";
                    } else {
                        activeDesc = base->ice_toString() + "  (checkedCast OK)";
                        std::cout << "  [OK] checkedCast - server now has the servant\n";
                    }
                    break;
                }
                case 'u': {
                    if (!base) { std::cout << "  [WARN] select a proxy first\n"; break; }
                    counter = Ice::uncheckedCast<Demo::CounterPrx>(base);
                    activeDesc = base->ice_toString() + "  (uncheckedCast)";
                    std::cout << "  [OK] uncheckedCast - no contact with the server yet\n";
                    break;
                }
                case 'g': {
                    if (!counter) { std::cout << "  [WARN] no Counter proxy (do c/u first)\n"; break; }
                    std::cout << "  value = " << counter->getValue() << "\n";
                    break;
                }
                case 'v': {
                    if (!counter) { std::cout << "  [WARN] no Counter proxy\n"; break; }
                    std::string s = readLine("  new value: ");
                    counter->setValue(std::stoi(s));
                    std::cout << "  [OK]\n";
                    break;
                }
                case 'i': {
                    if (!counter) { std::cout << "  [WARN] no Counter proxy\n"; break; }
                    std::cout << "  new value = " << counter->increment() << "\n";
                    break;
                }
                case 'r': {
                    if (!counter) { std::cout << "  [WARN] no Counter proxy\n"; break; }
                    counter->reset();
                    std::cout << "  [OK] reset\n";
                    break;
                }
                case 'n': {
                    if (!counter) { std::cout << "  [WARN] no Counter proxy\n"; break; }
                    std::cout << "  name = " << counter->getName() << "\n";
                    break;
                }
                case 'q':
                    return 0;
                default:
                    std::cout << "  [WARN] unknown command\n";
                }
            } catch (const Ice::Exception& ex) {
                std::cerr << "  [ICE-EX] " << ex.what() << "\n";
            } catch (const std::exception& ex) {
                std::cerr << "  [EX] " << ex.what() << "\n";
            }
        }
    } catch (const std::exception& ex) {
        std::cerr << "[FATAL] " << ex.what() << "\n";
        status = 1;
    }
    return status;
}
