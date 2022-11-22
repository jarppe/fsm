set dotenv-load := true
project := "fsm-ftw"


help:
  @just --list


# Start Node runtime so that Calva REPL can connect to it
node:
  node ./target/node.js


# Run tests
test focus=':unit' +opts="":
  @clear
  @echo "Running tests..."
  @clojure -M:test -m kaocha.runner                \
           --reporter kaocha.report/documentation  \
           --focus {{ focus }}                     \
           {{ opts }}


# Stop ShadowCljs
stop:
  npx shadow-cljs stop


# Start ShadowCljs
start:
  npx shadow-cljs start
