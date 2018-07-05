#!/bin/sh

set -ex

cargo +nightly build --target wasm32-unknown-unknown --release --verbose
#cargo +nightly run --manifest-path ../../crates/cli/Cargo.toml \
#  --bin wasm-bindgen -- \
#  ../../target/wasm32-unknown-unknown/debug/dom.wasm --out-dir .
wasm-bindgen target/wasm32-unknown-unknown/release/form_builder.wasm --out-dir . --browser
npm install
npm run serve
# wasm-snip --snip-rust-panicking-code --snip-rust-fmt-code form_builder_bg.wasm -o output.wasm && brotli ouput.wasm
