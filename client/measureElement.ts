import * as Types      from "./Types";
import defaultStyles   from "./default-styles.module.css"
import { cssPropertyNameToJavascriptPropertyName } from "./Utils";

export default function measureElement(spec: Types.MeasureSpec) : DOMRect {
  const container = document.createElement("div");
  container.className = defaultStyles.measurable;

  const e = document.createElement("span");
  if(spec.op === "MeasureText") {
    e.innerText = spec.text;
  } else if(spec.op === "MeasureHtml") {
    e.style.display = "inline-block";
    e.style.verticalAlign = "top";
    e.innerHTML = spec.contents;
  }
  for(const [key, value] of spec.styles) {
    (e.style as any)[cssPropertyNameToJavascriptPropertyName(key)] = value;
  }
  container.appendChild(e);

  document.body.appendChild(container);
  const bounds = e.getBoundingClientRect().toJSON();
  document.body.removeChild(container);

  return bounds;
}
