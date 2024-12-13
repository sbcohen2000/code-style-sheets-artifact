import * as React    from "react";
import * as Types    from "./Types"
import defaultStyles from "./default-styles.module.css"
import { cssPropertyNameToJavascriptPropertyName } from "./Utils";

interface StylishTextViewProps {
  layout: Types.LayoutData;
  inspectorIds: Set<number>;
  onClick?: (nodeId: number, event: React.MouseEvent) => void;
}

export default function StylishTextView(
  { layout, inspectorIds, onClick }: StylishTextViewProps) : JSX.Element {

    if(layout === undefined) {
      return <p>No layout data</p>;
    }

    const drawOrderLUT = constructDrawOrderMap(layout.metrics.drawOrder);

    return <div
             className={`${defaultStyles.measurable} ${defaultStyles.stylishStringRoot}`}>
             <TextLayer layout={layout} onClick={onClick}/>
             <BorderLayer
               onClick={(nodeId, e) => {
                 if(onClick !== undefined) {
                   onClick(nodeId, e);
                 }
               }}
               drawOrderLUT={drawOrderLUT}
               layout={layout}/>
             {
               inspectorIds.size === 0
                 ? <></>
                 : <InspectorLayer
                     onClick={(nodeId, e) => {
                       if(onClick !== undefined) {
                         onClick(nodeId, e);
                       }
                     }}
                     drawOrderLUT={drawOrderLUT}
                     inspectorIds={inspectorIds}
                     layout={layout}/>
             }
           </div>
  }

function buildStyleLUT(data: Types.LayoutData): Map<number, Types.ComputedStyle> {
  const lut = new Map<number, Types.ComputedStyle>();

  const recurse = (node: Types.FTTerm) => {
    lut.set(node.id, node.style);
    node.children.forEach(recurse);
  }

  recurse(data.root);
  return lut;
}

interface TextLayerProps {
  layout: Types.LayoutData;
  onClick: ((nodeId: number, event: React.MouseEvent) => void) | undefined;
}

function TextLayer({ layout, onClick }: TextLayerProps): JSX.Element {
  const fragments = layout.fragments;
  const metrics   = layout.metrics;
  const styleLut  = buildStyleLUT(layout);

  const lines:JSX.Element[] = [];
  let   line :JSX.Element[] = [];
  let   fragUniqueKeyCounters = new Map<number, number>();
  for(const [beforeGadgets, frag, afterGadgets] of fragments) {

    /* We need to give every fragment a unique key so that
     * React can keep track of them. We create stable keys
     * by choosing the ID of the parent, followed by the
     * index of the fragment. */
    const nextKey = (parentId: number): string => {
      let key = parentId.toString() + ".";
      const count = fragUniqueKeyCounters.get(parentId) ?? 0;
      key += (count + 1).toString();
      fragUniqueKeyCounters.set(parentId, count + 1);
      return key;
    }

    const insertGadget = (gadget: Types.Gadget) => {
      if(gadget.type === "GAdjustableShim") return;

      const g = <span
                  key={nextKey(gadget.id)}
                  style={{
                    width: gadget.width.toString() + "px",
                    display: "inline-block"
                  }}></span>;
      line.push(g);
    }

    beforeGadgets.forEach(insertGadget);

    let shouldBreakAfterThisFrag = false;

    switch(frag.type) {
      case "FText": {
        const otherStyles: Types.Styles =
          styleLut.get(frag.id)?.otherStyles ?? [];

        let props: React.CSSProperties = {};
        for(const [key, value] of otherStyles) {
          (props as any)[cssPropertyNameToJavascriptPropertyName(key)] = value;
        }
        const f = <span
                    onMouseDown={(event) => {
                      /* Prevent shift-click selections */
                      if(event.shiftKey) {
                        event.preventDefault();
                      }
                    }}
                    onClick={(event) => {
                      if(onClick) {
                        onClick(frag.id, event)
                      }
                    }}
                    key={nextKey(frag.id)}
                    style={props}>
                    {frag.text}
                  </span>;

        line.push(f);
      } break;
      case "FHtml": {
        const otherStyles: Types.Styles =
          styleLut.get(frag.id)?.otherStyles ?? [];

        let props: React.CSSProperties = {
          display: "inline-block",
          /* Note: verticalAlign should be set to either "top" or "bottom"
           * to prevent issues with layout. The layout algorithm assumes that
           * the height of a line is exactly the maximum of the heights of each
           * element on a line. Values of "top" and "bottom" maintain this
           * invariant. */
          verticalAlign: "top"
        };
        for(const [key, value] of otherStyles) {
          (props as any)[cssPropertyNameToJavascriptPropertyName(key)] = value;
        }
        const f = <span
                    onClick={(event) => {
                      if(onClick) {
                        onClick(frag.id, event)
                      }
                    }}
                    key={nextKey(frag.id)}
                    style={props}
                    dangerouslySetInnerHTML={{ __html: frag.contents }}>
                  </span>;
        line.push(f);
      } break;
      case "FNewline": {
        shouldBreakAfterThisFrag = true;
      } break;
    }

    afterGadgets.forEach(insertGadget);

    const lineSpacing = metrics.interLineSpacing[lines.length]!;

    if(shouldBreakAfterThisFrag) {
      lines.push(<div
                   key={"ln" + lines.length.toString()}
                   style={{
                     height: lineSpacing.lineHeight.toString() + "px",
                     paddingTop: lineSpacing.aboveSpace.toString() + "px",
                     paddingBottom: lineSpacing.belowSpace.toString() + "px",
                     position: "relative"
                   }}>{line}</div>);

      line = [];
    }
  }

  if(line.length !== 0) {
    const lineSpacing = metrics.interLineSpacing[lines.length]!;

    lines.push(<div
                 key={"ln" + lines.length.toString()}
                 style={{
                   height: lineSpacing.lineHeight.toString() + "px",
                   paddingTop: lineSpacing.aboveSpace.toString() + "px",
                   paddingBottom: lineSpacing.belowSpace.toString() + "px",
                   position: "relative"
                 }}>{line}</div>);
  }

  return <div style={{
    position: "relative",
    zIndex: 100
  }}>{lines}</div>
}

function constructDrawOrderMap(drawOrders: Array<[number, Types.DrawOrder[]]>) {
  const map = new Map<number, Types.DrawOrder[]>();
  for(const [id, orders] of drawOrders) {
    map.set(id, orders);
  }
  return map;
}

type BoundingRect = {
  minY: number;
  maxY: number;
  minX: number;
  maxX: number;
}

type Point = [number, number];

function makeBoundingRect(): BoundingRect {
  return {
    minY:  Infinity,
    maxY: -Infinity,
    minX:  Infinity,
    maxX: -Infinity
  }
}

function expandBoundingRect(rect: BoundingRect, point: Point): BoundingRect {
  return {
    minY: Math.min(rect.minY, point[1]),
    maxY: Math.max(rect.maxY, point[1]),
    minX: Math.min(rect.minX, point[0]),
    maxX: Math.max(rect.maxX, point[0])
  }
}

function inflateBoundingRect(rect: BoundingRect, amount: number): BoundingRect {
  return {
    minY: rect.minY - amount,
    maxY: rect.maxY + amount,
    minX: rect.minX - amount,
    maxX: rect.maxX + amount
  }
}

type Shape = Point[];
type Shapes = Shape[];

// check if any two neighboring points in Shape form an angle
// which is not 90 degrees.
function non90(points: Shape) {
  let last:null | Point = null;
  for(const [x, y] of points) {
    if(last === null) {
      last = [x, y];
    } else {
      const [x_1, y_1]:Point = last;
      if(x_1 !== x && x_1 !== y && y_1 !== x && y_1 !== y) {
        return true;
      }
      last = [x, y];
    }
  }
  return false;
}

function drawOrdersToBorderShapes(
  metrics: Types.LayoutMetrics,
  orders: Types.DrawOrder[],
  styles: Types.ComputedStyle): [Shapes, BoundingRect] {

    let ordersByShape: Types.HorzLine[][] = [[]];
    for(const order of orders) {
      if(order.type === "ClosePath") {
        ordersByShape.push([]);
      } else {
        ordersByShape[ordersByShape.length - 1]!.push(order);
      }
    }

    let shapes: Shapes = [];
    let contentRect = makeBoundingRect();

    for(const shapeOrders of ordersByShape) {
      shapes.push([]);
      for(let i = 0; i < shapeOrders.length; ++i) {
        const order = shapeOrders[i]!;
        const prevOrder = i === 0
          ? shapeOrders[shapeOrders.length - 1]!
          : shapeOrders[i - 1]!;
        const nextOrder = i === shapeOrders.length - 1
          ? shapeOrders[0]!
          : shapeOrders[i + 1]!;

        const prevOrderDoesFlip = prevOrder.span.flip !== order.span.flip;
        const nextOrderDoesFlip = nextOrder.span.flip !== order.span.flip;

        const xBoundingBegin = order.span.begin;
        const xBoundingEnd  = order.span.end;

        /* xOutsideBegin and xOutsideEnd mark the start and end
         * of the bounding rect of the node, but we want to draw
         * the border within the content rect. So, we compute these
         * new x values to find the x-offset of the border.
         *
         * We also must handle inside edges specially since on the "top"
         * of a tetris block, we need the vertical segment to move to the
         * right, while the vertical segment needs to move to the left on
         * the "bottom" of the tetris block.
         *
         *                            \/ inside edge
         *                           _______
         *  vertical segment ->  ____|   __|
         *  on "top"inside edge  |_______| <- vertical segment on
         *                       ^       ^    "bottom" inside edge
         *                       outside edges
         */
        const borderOffsetAmount = styles.margin + styles.borderWidth / 2;

        /* happy path: we're on an outside edge */
        let xBorderBegin = xBoundingBegin + borderOffsetAmount;
        let xBorderEnd = xBoundingEnd - borderOffsetAmount;

        /* less happy path: we have encountered an inside edge.
         * We need to move the endpoint of the current segment
         * accordingly */
        if(!order.span.flip && !nextOrderDoesFlip) {
          /* *forward* -> foward */
          xBorderEnd = xBoundingEnd + borderOffsetAmount;
        }
        else if(order.span.flip && !nextOrderDoesFlip) {
          /* backward <- *backward* */
          xBorderBegin = xBoundingBegin - borderOffsetAmount;
        }
        else if(!order.span.flip && !prevOrderDoesFlip) {
          /* *forward* -> forward */
          xBorderBegin = xBoundingBegin + borderOffsetAmount;
        }
        else if(order.span.flip && !prevOrderDoesFlip) {
          /* *backward* <- backward */
          xBorderEnd = xBoundingEnd - borderOffsetAmount;
        }

        const lineSpacing = metrics.interLineSpacing[order.line]!;

        /* Similar to the horizontal axis, we want to draw the border
         * outlining the content rect, not the bounding rect. So, we
         * do some math here to figure out the y-offset of the border. */
        const topEdgeOfText = lineSpacing.top + lineSpacing.aboveSpace;
        const yOutside = order.side == "above"
	  ? topEdgeOfText - order.offset
	  : topEdgeOfText + lineSpacing.lineHeight + order.offset;

        const yBorder = order.side == "above"
	  ? yOutside + (styles.margin + styles.borderWidth / 2)
	  : yOutside - (styles.margin + styles.borderWidth / 2);


        if(order.span.flip) {
          shapes[shapes.length - 1]!.push([xBorderEnd, yBorder]);
          shapes[shapes.length - 1]!.push([xBorderBegin, yBorder]);
        } else {
          shapes[shapes.length - 1]!.push([xBorderBegin, yBorder]);
          shapes[shapes.length - 1]!.push([xBorderEnd, yBorder]);
        }

        contentRect = expandBoundingRect(contentRect, [xBorderBegin, yBorder]);
        contentRect = expandBoundingRect(contentRect, [xBorderEnd,   yBorder]);
      }
    }

    shapes = shapes.filter(ps => ps.length > 0);

    // This is a check which should help expose bugs in the layout
    // algorithm.
    for(const shape of shapes) {
      if(non90(shape)) {
        console.warn("draw orders cause invalid shape:", orders);
      }
    }

    return [shapes, contentRect];
  }

function shapesToSvgPathString(shapes: Shapes): string {
  let pathString : string = "";
  for(const shape of shapes) {
    let shapeString : string | undefined;
    for(const [x, y] of shape) {
      if(shapeString === undefined) {
	shapeString  = `M ${x} ${y} `
      } else {
	shapeString += `L ${x} ${y} `
      }
    }
    pathString += shapeString + "z ";
  }
  return pathString;
}

function pointsToRoundedSvgPathString(
  points: Shape,
  radius: number): string {
    const lerp = ([bx, by]: Point, [ex, ey]: Point, t: number): Point => {
      const [dx, dy] = [ex - bx, ey - by];
      return [bx + dx * t, by + dy * t];
    }

    const distance = ([bx, by]: Point, [ex, ey]: Point): number =>
      Math.sqrt(Math.pow(bx - ex, 2) + Math.pow(by - ey, 2));

    const lerpD = (p1: Point, p2: Point, d: number): Point => {
      const maxD = distance(p1, p2);
      if(Math.abs(maxD) < 0.0001) return p1;
      return lerp(p1, p2, d / maxD);
    }

    const cross = ([ax, ay]: Point, [bx, by]: Point, [cx, cy]: Point): number => {
      const [px, py] = [bx - ax, by - ay];
      const [qx, qy] = [cx - bx, cy - by];
      return px * qy - py * qx;
    }

    let pathString = undefined;
    for(let i = 0; i < points.length; ++i) {
      const curr = points[i];
      const next = i === points.length - 1 ? points[0] : points[i + 1];
      const prev = i === 0 ? points[points.length - 1] : points[i - 1];

      const d = distance(next!, points[i]!);
      const r = Math.min(radius, d / 2);

      const a1 = lerpD(curr!, prev!, r);
      if(pathString === undefined) {
        pathString  = `M ${a1[0]} ${a1[1]} `;
      } else {
        pathString += `L ${a1[0]} ${a1[1]} `;
      }

      const dir = Math.max(Math.min(cross(prev!, curr!, next!), 1), 0);

      const a2 = lerpD(curr!, next!, r);
      pathString += `A ${r} ${r} 0 0 ${dir} ${a2[0]} ${a2[1]}`
    }

    return pathString + "z ";
  }

function shapesToRoundedSvgPathString(shapes: Shapes, radius: number): string {
  let pathString = "";
  for(const shape of shapes) {
    pathString += pointsToRoundedSvgPathString(shape, radius);
  }
  return pathString;
}

interface SVGBorderProps {
  id: number;
  metrics: Types.LayoutMetrics;
  orders: Types.DrawOrder[];
  style: Types.ComputedStyle;
  onClick: (nodeId: number, event: React.MouseEvent) => void;
}

function SVGBorder({ id, metrics, orders, style, onClick }: SVGBorderProps): JSX.Element {
  // FIXME: This pass should be broken into two separate
  //        passes. We need to draw all of the backgrounds,
  //        then all of the text, then all of the borders.

  let [shapes, bounds] = drawOrdersToBorderShapes(metrics, orders, style);
  bounds = inflateBoundingRect(bounds, style.borderWidth);

  const w = bounds.maxX - bounds.minX;
  const h = bounds.maxY - bounds.minY;

  const svg =
    <svg
      onClick={(e) => onClick(id, e)}
      key={id.toString()}
      viewBox={`${bounds.minX} ${bounds.minY} ${w} ${h}`}
      style={{
        position: "absolute",
        top: bounds.minY + "px",
        left: bounds.minX + "px"
      }}
      width={w + "px"}
      height={h + "px"}
      stroke={style.borderColor}
      fill={style.backgroundColor}
      strokeWidth={style.borderWidth}>
      <path d={shapesToRoundedSvgPathString(shapes, style.borderRadius)}/>
    </svg>;

  return svg;
}

interface BorderLayerProps {
  layout: Types.LayoutData;
  drawOrderLUT: Map<number, Types.DrawOrder[]>;
  onClick: (nodeId: number, event: React.MouseEvent) => void;
}

function BorderLayer({ layout, drawOrderLUT, onClick }: BorderLayerProps): JSX.Element {
  const root    = layout.root;
  const metrics = layout.metrics;

  const paths: JSX.Element[] = [];
  const recurse = (node: Types.FTTerm) => {
    const orders = drawOrderLUT.get(node.id);
    if(orders !== undefined) {
      paths.push(<SVGBorder
                   key={node.id.toString()}
                   id={node.id}
                   metrics={metrics}
                   orders={orders}
                   style={node.style}
                   onClick={onClick}/>);
    }

    node.children.forEach(recurse);
  }

  recurse(root);

  return <div style={{
    position: "absolute",
    top: 0,
    left: 0,
    zIndex: 0
  }}>{paths}</div>
}

function SVGInspectorBorder({ id, metrics, orders, style, onClick }: SVGBorderProps): JSX.Element {
  let [shapes, bounds] = drawOrdersToBorderShapes(metrics, orders, style);
  bounds = inflateBoundingRect(bounds, 1); /* 1 px */

  const w = bounds.maxX - bounds.minX;
  const h = bounds.maxY - bounds.minY;

  const svg = <svg
                onClick={(e) => onClick(id, e)}
                key={id.toString()}
                viewBox={`${bounds.minX} ${bounds.minY} ${w} ${h}`}
                style={{
                  position: "absolute",
                  top: bounds.minY + "px",
                  left: bounds.minX + "px"
                }}
                width={w + "px"}
                height={h + "px"}
                fill={"none"}
                stroke={"rgba(255, 0, 255, 1)"}
                strokeWidth={`2px`}>
                <path d={shapesToSvgPathString(shapes)}/>
              </svg>;

  return svg;
}

interface InspectorLayerProps {
  layout: Types.LayoutData;
  inspectorIds: Set<number>;
  drawOrderLUT: Map<number, Types.DrawOrder[]>;
  onClick: (nodeId: number, event: React.MouseEvent) => void;
}

function InspectorLayer(props: InspectorLayerProps): JSX.Element {
  const root    = props.layout.root;
  const metrics = props.layout.metrics;

  const paths: JSX.Element[] = [];
  const recurse = (node: Types.FTTerm) => {
    if(props.inspectorIds.has(node.id)) {
      const orders = props.drawOrderLUT.get(node.id);
      if(orders !== undefined) {
        paths.push(<SVGInspectorBorder
                     key={node.id.toString()}
                     id={node.id}
                     metrics={metrics}
                     orders={orders}
                     style={node.style}
                     onClick={props.onClick}/>);
      }
    }

    node.children.forEach(recurse);
  }

  recurse(root);

  return <div style={{
    position: "absolute",
    top: 0,
    left: 0,
    zIndex: 200
  }}>{paths}</div>
}
