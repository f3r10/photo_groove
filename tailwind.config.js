// tool to generate palette https://ockam.github.io/tailwind-palette/
const colors = {
  "gv-primary-darkest": "#003f53",
  "gv-primary-darker": "#00586d",
  "gv-primary-dark": "#27859b",
  "gv-primary": "#60b5cc",
  "gv-primary-light": "#94e7ff",
  "gv-primary-lighter": "#c8ffff",
  "gv-primary-lightest": "#e3ffff",
};
export default {
  purge: [],
  darkMode: false, // or 'media' or 'class'
  theme: {
    extend: {
      colors: {
        ...colors,
      },
    },
  },
  variants: [],
  plugins: [],
};
