import * as React from "react";
import { useRef, useLayoutEffect, useState, useEffect, ReactElement } from "react";
import { createPortal } from "react-dom";

interface FloatingComponentProps {
  x: number;
  y: number;
  children: ReactElement;
  onShouldClose?: () => void;
  zIndex?: number;
}

export default function FloatingComponent(props: FloatingComponentProps): JSX.Element {
  const ref = useRef<HTMLDivElement>(null);
  const [rect, setRect] = useState<DOMRect | null>(null);

  useLayoutEffect(() => {
    if(ref.current === null) return;
    setRect(ref.current.getBoundingClientRect());
  }, [props.children]);

  useEffect(() => {
    function onMouseDown(event: MouseEvent) {
      if(rect === null) return;

      const mouseIsInside = event.clientX > rect.left
                         && event.clientX < rect.right
                         && event.clientY > rect.top
                         && event.clientY < rect.bottom;
      if(!mouseIsInside && props.onShouldClose !== undefined) {
        props.onShouldClose();
      }
    }

    window.addEventListener("mousedown", onMouseDown);
    return () => {
      window.removeEventListener("mousedown", onMouseDown);
    }
  }, [rect]);

  return createPortal(<div ref={ref} style={{
    zIndex: props.zIndex,
    position: 'absolute',
    left: `${props.x}px`,
    top: `${props.y}px`,
  }}>
    { props.children }
  </div>, document.body);
}
