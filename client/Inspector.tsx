import styles from "./inspector.module.css";
import * as React from "react";
import { useEffect, useState, useRef } from "react";
import * as Types from "./Types";
import { assert } from "./common";

interface InspectorProps {
  ids: Set<number>;
  layout: Types.LayoutData;
  onClick?: (nodeId: number, event: React.MouseEvent) => void;
}

type PathInfo = {
  lca: Types.FTTerm;
  paths: number[][];
}

/* Find a path from idA and idB's lowest common ancestor to idA and idB. */
type TargetInfo = {
  level: number;
  node: Types.FTTerm;
  pathUp: number[];
}
function findPath(data: Types.LayoutData, ids: Set<number>): PathInfo {
  assert(ids.size >= 1);

  const parentLUT = new Map<number, { childIndex: number, parent: Types.FTTerm }>;
  const infoLUT = new Map<number, TargetInfo>();

  const fillLUTs = (node: Types.FTTerm, parent: Types.FTTerm | null, childIndex: number, level: number) => {
    for(const id of ids) {
      if(node.id === id) {
        infoLUT.set(id, {
          level,
          node,
          pathUp: []
        });
      }
    }
    if(parent !== null) {
      parentLUT.set(node.id, { childIndex, parent });
    }

    for(let i = 0; i < node.children.length; ++i) {
      fillLUTs(node.children[i]!, node, i, level + 1);
    }
  }
  fillLUTs(data.root, null, -1, 0);

  let topmost: TargetInfo = infoLUT.get(ids.values().next().value!)!;
  for(const [_id, info] of infoLUT) {
    if(info.level < topmost.level) {
      topmost = info;
    }
  }

  /* Move each node up so it's on the same level as topmost */
  for(const [id, info] of infoLUT) {
    if(id === topmost.node.id) continue;

    let diff = Math.abs(topmost.level - info.level);

    for(let i = 0; i < diff; ++i) {
      let p = parentLUT.get(info.node.id)!;
      info.node = p.parent;
      info.pathUp.unshift(p.childIndex);
    }
  }

  for(;;) {
    /* Iterate until every node in infoLUT is the same */
    let done: boolean = true;
    for(const [_id, info] of infoLUT) {
      done = done && info.node === topmost.node;
    }

    if(done) break;

    for(const [_id, info] of infoLUT) {
      const p = parentLUT.get(info.node.id)!;
      info.node = p.parent;
      info.pathUp.unshift(p.childIndex);
    }
  }

  const paths: number[][] = [];
  for(const [_id, info] of infoLUT) {
    paths.push(info.pathUp);
  }

  return { lca: topmost.node, paths };
}

export default function Inspector(props: InspectorProps): JSX.Element {
  const [pathInfo, setPathInfo] = useState<PathInfo | null>(null);
  const pathToColorLUT = useRef<{
    map: Map<number[], [string, boolean]>,
    nextColorIndex: number
  }>({
    map: new Map(),
    nextColorIndex: 0
  });

  function getColorOfPath(path: number[]): [string, boolean] {
    const res = pathToColorLUT.current.map.get(path);
    if(res !== undefined) return res;

    if(pathToColorLUT.current.nextColorIndex >= ALLOWED_COLORS.length) {
      pathToColorLUT.current.nextColorIndex = 0;
    }

    const color = ALLOWED_COLORS[pathToColorLUT.current.nextColorIndex++]!;
    pathToColorLUT.current.map.set(path, color);
    return color;
  }

  useEffect(() => {
    if(props.ids.size > 0) {
      setPathInfo(findPath(props.layout, props.ids));
    } else {
      setPathInfo(null);
    }
  }, [props.ids]);

  if(pathInfo === null) return <></>;

  return <NodeSummaryComponent
           selectedIds={props.ids}
           fragments={props.layout.fragments}
           pathInfo={pathInfo}
           onNeedPathColor={getColorOfPath}
           onClick={(nodeId: number, e: React.MouseEvent) => {
             if(props.onClick !== undefined) {
               props.onClick(nodeId, e);
             }
           }}/>;
}

interface NodeSummaryComponentProps {
  selectedIds: Set<number>;
  fragments: Types.Fragments;
  pathInfo: PathInfo;
  onNeedPathColor: (path: number[]) => [string, boolean];
  onClick: (nodeId: number, event: React.MouseEvent) => void;
}

function NodeSummaryComponent(props: NodeSummaryComponentProps): JSX.Element {
  return <ul><NodeSummaryComponentImpl
               selectedIds={props.selectedIds}
               fragments={props.fragments}
               pathInfo={props.pathInfo}
               onNeedPathColor={props.onNeedPathColor}
               onClick={props.onClick}/></ul>;
}

function NodeSummaryComponentImpl(props: NodeSummaryComponentProps): JSX.Element {
  let fragUniqueKeyCounters = new Map<number, number>();
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

  const childComponents: JSX.Element[] = [];
  const fragsPreview: JSX.Element[] = [];

  let currentFrag = props.pathInfo.lca.fragments.begin;
  let currentEnd  = props.pathInfo.lca.fragments.end;


  /* Group paths into lists where each path in
   * a list starts with the same index. */
  const pathGroups = new Map<number, number[][]>();
  for(const path of props.pathInfo.paths) {
    if(path.length === 0) continue;

    const existing = pathGroups.get(path[0]!);
    if(existing === undefined) {
      pathGroups.set(path[0]!, [path.slice(1)]);
    } else {
      pathGroups.set(path[0]!, existing.concat([path.slice(1)]));
    }
  }

  for(const [front, pathGroup] of pathGroups) {
    const child = props.pathInfo.lca.children[front]!;

    const b = child.fragments.begin;
    currentEnd = child.fragments.end;

    if(currentFrag < b) {
      for(let i = currentFrag; i < b; ++i) {
        fragsPreview.push(
          <FragmentPreview
            key={nextKey(child.id)}
            fragment={props.fragments[i]![1]}/>
        );
      }
    }

    const newPathInfo = {
      lca: child,
      paths: pathGroup
    }
    childComponents.push(
      <NodeSummaryComponentImpl
        key={nextKey(child.id)}
        selectedIds={props.selectedIds}
        fragments={props.fragments}
        pathInfo={newPathInfo}
        onNeedPathColor={props.onNeedPathColor}
        onClick={props.onClick}/>
    );

    currentFrag = child.fragments.end;
  }

  if(currentFrag < currentEnd) {
    for(let i = currentFrag; i < currentEnd; ++i) {
      fragsPreview.push(
        <FragmentPreview
          key={nextKey(props.pathInfo.lca.id)}
          fragment={props.fragments[i]![1]}/>
      );
    }
  }

  const path: number[] | null = props.pathInfo.lca.style.inspectorInfo.path;
  const pathColor: [string, boolean] = path === null ? ["transparent", false] : props.onNeedPathColor(path);

  const hasAnyFragments: boolean = fragsPreview.length !== 0;
  const hasAnyClasses: boolean = props.pathInfo.lca.style.inspectorInfo.classes.length !== 0;

  const classesPreview: JSX.Element =
    <span className={styles.class_preview}>
      {
        props.pathInfo.lca.style.inspectorInfo.classes
          .reduce((rest, cls) => rest + " " + cls, "")
      }
    </span>

  const rootStyles = {
    backgroundColor: pathColor[0],
    border: props.selectedIds.has(props.pathInfo.lca.id) ? "2px solid magenta" : "none"
  }

  const rootClasses = [styles.summary_root];
  if(pathColor[1]) {
    rootClasses.push(styles.inverted_colors);
  }
  if(path !== null) {
    rootClasses.push(styles.colorful);
  }

  const pathString: string =
    props.pathInfo.lca.style.inspectorInfo.path === null
      ? "None"
      : props.pathInfo.lca.style.inspectorInfo.path.join(",");

  const summary =
    <div
      onClick={(e) => props.onClick(props.pathInfo.lca.id, e)}
      onMouseDown={(event) => {
        /* Prevent shift-click selections */
        if(event.shiftKey) {
          event.preventDefault();
        }
      }}
      style={rootStyles}
      className={rootClasses.join(" ")}>
      <span style={{ gridColumn: 1, gridRow: 1 }}>path:</span>
      <div  style={{ gridColumn: 2, gridRow: 1 }}><PathPreview path={pathString}/></div>
      <span style={{ gridColumn: 1, gridRow: 2 }} className={hasAnyFragments ? "" : styles.grayed_out}>fragments below:</span>
      <div  style={{ gridColumn: 2, gridRow: 2 }}>{ fragsPreview }</div>
      <span style={{ gridColumn: 1, gridRow: 3 }} className={hasAnyClasses ? "" : styles.grayed_out}>classes:</span>
      <div  style={{ gridColumn: 2, gridRow: 3 }}>{ classesPreview }</div>
    </div>;

  if(childComponents.length === 0) {
    return <li>{ summary }</li>
  } else {
    return <li>
             { summary }
             <ul>
               { childComponents }
             </ul>
           </li>
  }
}

interface FragmentPreviewProps {
  fragment: Types.Fragment
}

function FragmentPreview({ fragment }: FragmentPreviewProps): JSX.Element {
  switch(fragment.type) {
    case "FNewline": {
      return <span className={`${styles.non_printable} ${styles.newline}`}>
               {'\\n'}</span>
    }
    case "FText": {
      return <span className={`${styles.text_fragment}`}>
               { fragment.text }</span>
    }
    case "FHtml": {
      return <span className={`${styles.html_fragment}`}>HTML</span>
    }
  }
}

interface PathPreviewProps {
  path: string;
}

function PathPreview({ path }: PathPreviewProps) {
  const [isVisible, setIsVisible] = useState<boolean>(false);

  function toggleVisibility(event: React.MouseEvent) {
    event.stopPropagation();
    setIsVisible(visibility => !visibility);
  }

  if(isVisible || path.length <= 10) {
    return <span
             onClick={toggleVisibility}>
             {path}
           </span>
  } else {
    const amount = Math.min(10, path.length);
    return <span
             onClick={toggleVisibility}>
             {"..." + path.slice(path.length - amount)}
           </span>
  }
}

/* Note: first column: the color. Second column: does this color
 * require that text be inverted above it? */
const ALLOWED_COLORS: [string, boolean][] = [
  ["#EFCDB8", false],
  ["#1CD3A2", true],
  ["#979AAA", true],
  ["#DE5D83", false],
  ["#EFDBC5", false],
  ["#78DBE2", false],
  ["#414A4C", true],
  ["#F780A1", false],
  ["#FAE7B5", false],
  ["#ADADD6", false],
  ["#CC6666", false],
  ["#FFBCD9", false],
  ["#FFFF99", false],
  ["#7442C8", true],
  ["#CD9575", false],
  ["#FDD7E4", false],
  ["#71BC78", false],
  ["#8F509D", true],
  ["#9F8170", false],
  ["#FD5E53", false],
  ["#30BA8F", false],
  ["#6E5160", true],
  ["#8A795D", true]
];
