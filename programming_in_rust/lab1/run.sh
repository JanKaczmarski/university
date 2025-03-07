set -eo pipefail

rustc main.rs
./main
rm main
