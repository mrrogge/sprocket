import * as wasm from "wasm-sprocket";

const source_el = document.getElementById("sprocket-source");
const build_btn = document.getElementById("build-btn");
const dest_el = document.getElementById("sprocket-ast");

build_btn.addEventListener("click", () => {
    dest_el.textContent = wasm.parse(source_el.value);
})
