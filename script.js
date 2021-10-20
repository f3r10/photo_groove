class RangeSlider extends HTMLElement {
  connectedCallback() {
    var input = document.createElement("input");
    this.appendChild(input);
  }
}
window.customElements.define("range-slider", RangeSlider);
