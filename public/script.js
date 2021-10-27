class RangeSlider extends HTMLElement {
  connectedCallback() {
    var input = document.createElement("input");
    this.appendChild(input);

    var rangeSliderNode = this;

    var jsr = new JSR(input, {
      max: this.max,
      values: [this.val],
      sliders: 1,
      grid: false,
    });

    var a = rangeSliderNode.getElementsByClassName("jsr_label");
    console.log("jsr_label", typeof a);
    for (let i = 0; i < a.length; i++) {
      a[i].style["display"] = "none";
    }

    jsr.addEventListener("update", function (elem, value) {
      var event = new CustomEvent("slide", {
        detail: { userSlidTo: value },
      });
      rangeSliderNode.dispatchEvent(event);
    });
  }
}
window.customElements.define("range-slider", RangeSlider);
