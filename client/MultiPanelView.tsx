import * as React from "react";
import { useState, useRef, useEffect } from "react";
import styles from "./multi-panel-view.module.css";
import FloatingComponent from "./FloatingComponent";
import { assert } from "./common";

type Direction = "horizontal" | "vertical";

type ViewTreeNode = {
  type: "ViewTreeNode";
  id: number;
  direction: Direction;
  t: number;
  left: ViewTree;
  right: ViewTree;
}

type ViewTreeLeaf = {
  type: "ViewTreeLeaf";
  id: number;
  childIds: string[];
  selectedChildId: string | null;
}

type ViewTree = ViewTreeNode | ViewTreeLeaf;

export type Model = {
  tree: ViewTree,
  nextFreshId: number
}

export function newModel(): Model {
  return {
    tree: emptyViewTree(0, []),
    nextFreshId: 1
  }
}

export function addChild(childId: string, model: Model): Model {
  /* add the child to the leftmost leaf we can find */
  let node: ViewTree = model.tree;
  while(node.type !== "ViewTreeLeaf") {
    node = node.left;
  }

  const newTree = copyAndReplace(node.id, (node: ViewTree) => {
    assert(node.type === "ViewTreeLeaf");
    const newChildIds = node.childIds.concat(childId);
    return {
      ...node,
      childIds: newChildIds,
      selectedChildId: node.selectedChildId ?? childId
    };
  }, model.tree);

  return { ...model, tree: newTree };
}

export function removeChild(childId: string, model: Model): Model {
  const res = findLeafWithChildId(childId, model);
  if(res === undefined) return model;

  let newTree: ViewTree;
  newTree = copyAndReplace(res.id, (node: ViewTree) => {
    assert(node.type === "ViewTreeLeaf");
    const newChildIds = node.childIds.filter(id => id !== childId);
    return {
      ...node,
      childIds: newChildIds,
      selectedChildId: newChildIds[0] ?? null
    };
  }, model.tree);

  if(res.type === "ViewTreeLeaf" && res.childIds.length === 1) {
    /* Removed the last child, remove the view from the tree */
    newTree = copyAndRemove(res.id, newTree);
  }

  return { ...model, tree: newTree };
}

export function selectChild(childId: string, model: Model): Model {
  const res = findLeafWithChildId(childId, model);
  if(res === undefined) return model;

  return {
    ...model,
    tree: copyAndReplace(res.id, (target: ViewTree) => {
      assert(target.type === "ViewTreeLeaf");
      return { ...target, selectedChildId: childId };
    }, model.tree)
  };
}

function findLeafWithChildId(childId: string, model: Model): ViewTree | undefined {
  return findNode((node: ViewTree) => {
    if(node.type === "ViewTreeNode") return false;
    return node.childIds.indexOf(childId) !== -1;
  }, model.tree);
}

function freshId(model: Model): [number, Model] {
  const id = model.nextFreshId;
  const m = { ...model, nextFreshId: model.nextFreshId + 1 }
  return [id, m];
}

function emptyViewTree(id: number, childIds: string[]): ViewTree {
  return { type: "ViewTreeLeaf", id, childIds, selectedChildId: null }
}

function findNode(f: (node: ViewTree) => boolean, tree: ViewTree): ViewTree | undefined {
  if(f(tree)) {
    return tree
  }

  if(tree.type === "ViewTreeNode") {
    const leftRes = findNode(f, tree.left);
    if(leftRes !== undefined) {
      return leftRes;
    }
    const rightRes = findNode(f, tree.right);
    if(rightRes !== undefined) {
      return rightRes;
    }
  }

  return undefined;
}

function copyAndReplace(id: number, f: (node: ViewTree) => ViewTree, tree: ViewTree): ViewTree {
  let copy = { ...tree };
  if(copy.type === "ViewTreeNode" && tree.type === "ViewTreeNode") {
    copy.left = copyAndReplace(id, f, tree.left);
    copy.right = copyAndReplace(id, f, tree.right);
  }

  if(tree.id === id) {
    return f(copy);
  } else {
    return copy;
  }
}

function copyAndRemove(id: number, tree: ViewTree): ViewTree {
  if(tree.type === "ViewTreeNode") {
    if(tree.left.id === id) {
      return tree.right;
    } else if(tree.right.id === id) {
      return tree.left;
    } else {
      return {
        ...tree,
        left: copyAndRemove(id, tree.left),
        right: copyAndRemove(id, tree.right)
      }
    }
  } else {
    /* can't remove root */
    return { ...tree };
  }
}

interface MultiPanelViewProps {
  model: Model;
  onModelChanged: (newModel: Model) => void;
  onContextMenu: (clientX: number, clientY: number, childId: string) => void;
  children: Array<React.ReactElement<ViewProps>> | React.ReactElement<ViewProps>;
}

export function MultiPanelView(props: MultiPanelViewProps) : JSX.Element {
  const onPointerMove = useRef<((event: PointerEvent) => void) | null>(null);
  /* currentMove tracks the current divider drag movement, if any */
  const [currentMove, setCurrentMove] = useState<{ nodeId: number, t: number } | null>(null);
  /* dragPreview tracks id of the leaf under the currently dragging tab, if any */
  const [dragPreview, setDragPreview] = useState<{ leafId: number } | null>(null);
  const viewSizeLUT = useRef<Map<number, { rect: DOMRect, seen: boolean }>>(new Map());

  /* Remove stale entries from the viewSizeLUT */
  useEffect(() => {
    viewSizeLUT.current.forEach(value => {
      value.seen = false;
    });

    function f(tree: ViewTree) {
      const possibleEntry = viewSizeLUT.current.get(tree.id);
      if(possibleEntry !== undefined) {
        viewSizeLUT.current.set(tree.id, { ...possibleEntry, seen: true });
      }

      if(tree.type === "ViewTreeNode") {
        f(tree.left);
        f(tree.right);
      }
    }

    f(props.model.tree);

    for(const [key, value] of viewSizeLUT.current.entries()) {
      if(!value.seen) {
        viewSizeLUT.current.delete(key);
      }
    }
  }, [props.model]);

  function onSplit(direction: Direction, id: number) {
    const [childId, m1] = freshId(props.model);
    const [parentId, m2] = freshId(m1);

    const tree = copyAndReplace(id, (target: ViewTree) => {
      if(target.type === "ViewTreeNode") return target;

      const newChild = emptyViewTree(childId, []);
      const newParent: ViewTree = {
        type: "ViewTreeNode",
        id: parentId,
        direction,
        t: 0.5,
        left: target,
        right: newChild,
      }

      return newParent;
    }, m2.tree);
    props.onModelChanged({ ...m2, tree: tree });
  }

  function onClose(id: number) {
    props.onModelChanged({
      ...props.model,
      tree: copyAndRemove(id, props.model.tree)
    })
  }

  function onTabSelected(id: string, leafId: number) {
    props.onModelChanged({
      ...props.model,
      tree: copyAndReplace(leafId, (target: ViewTree) => {
        assert(target.type === "ViewTreeLeaf");
        return { ...target, selectedChildId: id };
      }, props.model.tree)
    })
  }

  function onPointerDown(dir: Direction, id: number, event: React.PointerEvent) {
    event.currentTarget.setPointerCapture(event.pointerId);
    const parent = event.currentTarget.parentElement!;
    /* ensure that parent is a view root element */
    assert(parent.classList.contains(styles.view_root));
    const bounds = parent.getBoundingClientRect();

    onPointerMove.current = (event: PointerEvent) => {
      let t: number;
      if(dir === "horizontal") {
        t = clamp((event.x - bounds.left) / bounds.width, 0, 1);
      } else {
        t = clamp((event.y - bounds.top) / bounds.height, 0, 1);
      }
      setCurrentMove({ nodeId: id, t });
    };
    window.addEventListener("pointermove", onPointerMove.current);
  }

  function onPointerUp(_id: number, event: React.PointerEvent) {
    event.currentTarget.releasePointerCapture(event.pointerId);
    if(onPointerMove.current !== null) {
      window.removeEventListener("pointermove", onPointerMove.current);
      onPointerMove.current = null;

      if(currentMove !== null) {
        props.onModelChanged({
          ...props.model,
          tree: copyAndReplace(currentMove.nodeId, (target: ViewTree) => {
            return { ...target, t: currentMove.t };
          }, props.model.tree)
        });

        setCurrentMove(null);
      }
    }
  }

  function onSize(rect: DOMRect, leafId: number) {
    /* Note: we can set seen to false here since it will
     * be reset before being checked in the above useEffect. */
    viewSizeLUT.current.set(leafId, { rect, seen: false });
  }

  function leafIdUnderPoint(clientX: number, clientY: number): number | null {
    for(const [key, value] of viewSizeLUT.current.entries()) {
      if(clientX > value.rect.left && clientX < value.rect.right
        && clientY > value.rect.top && clientY < value.rect.bottom) {
          return key;
        }
    }
    return null;
  }

  function onDrop(clientX: number, clientY: number, childId: string, treeId: number) {
    const targetLeafId = leafIdUnderPoint(clientX, clientY);
    if(targetLeafId === null) return;
    if(targetLeafId === treeId) return; /* Dragging onto self is a noop */

    const m1 = removeChild(childId, props.model);
    /* insert child under targetLeaf */
    const newTree = copyAndReplace(targetLeafId, (node: ViewTree) => {
      assert(node.type === "ViewTreeLeaf");
      return {
        ...node,
        childIds: node.childIds.concat(childId),
        selectedChildId: node.selectedChildId ?? childId
      };
    }, m1.tree);
    props.onModelChanged({ ...m1, tree: newTree });
    setDragPreview(null);
  }

  function updateDragPreview(clientX: number, clientY: number, _childId: string, treeId: number) {
    const targetLeafId = leafIdUnderPoint(clientX, clientY);
    if(targetLeafId === null || targetLeafId === treeId) {
      setDragPreview(null);
    } else if(dragPreview === null ||
      (dragPreview !== null && dragPreview.leafId !== targetLeafId)) {
        setDragPreview({ leafId: targetLeafId });
      }
  }

  function buildTree(tree: ViewTree) : JSX.Element {
    if(tree.type === "ViewTreeLeaf") {
      const children = Array.isArray(props.children) ? props.children : [props.children]
      const inLeaf = children.filter(child => tree.childIds.indexOf(child.props.id) !== -1)
      const selected = inLeaf.find(child => tree.selectedChildId === child.props.id)

      let child_component;
      if(selected === undefined && tree.selectedChildId === null) {
        /* There is no selected tab on this view */
        child_component = <div className={styles.info_message}><p>No tab is selected</p></div>;
      } else if(selected === undefined && tree.selectedChildId !== null) {
        /* The child was removed from the props, but not the model */
        child_component = <div className={styles.info_message}><p>Missing child</p></div>;
      } else if(selected !== undefined) {
        child_component = selected;
      }

      const showDragPreview = dragPreview !== null
        && dragPreview.leafId === tree.id;

      return <div key={tree.id.toString()}
               className={styles.view_root + (showDragPreview ? " " + styles.drag_preview : "")}>
               <SizeInformer onSize={(rect: DOMRect) => onSize(rect, tree.id)}>
                 { child_component }
               </SizeInformer>
               <TabBar records={inLeaf.map(child => ({ label: child.props.label, id: child.props.id }))}
                 onTabSelected={(id: string) => onTabSelected(id, tree.id)}
                 onSplitHorz={() => onSplit("horizontal", tree.id)}
                 onSplitVert={() => onSplit("vertical", tree.id)}
                 onClose={() => onClose(tree.id)}
                 onDrop={(clientX, clientY, childId) => onDrop(clientX, clientY, childId, tree.id)}
                 onShouldUpdateDragPreview={(clientX, clientY, childId) => updateDragPreview(clientX, clientY, childId, tree.id)}
                 selectedTabId={tree.selectedChildId}
                 onContextMenu={props.onContextMenu}/>
             </div>
    } else {
      let t = tree.t;
      if(currentMove !== null && currentMove.nodeId === tree.id) {
        t = currentMove.t;
      }

      const left_style =
        tree.direction === "horizontal"
          ? {
            width: (t * 100).toString() + "%",
            height: "100%",
          }
          : {
            width: "100%",
            height: (t * 100).toString() + "%",
          }

      const right_style =
        tree.direction === "horizontal"
          ? {
            width: ((1 - t) * 100).toString() + "%",
            height: "100%",
          }
          : {
            width: "100%",
            height: ((1 - t) * 100).toString() + "%",
          }

      const style = tree.direction === "horizontal"
        ? { flexDirection: "row" as const }
        : {  flexDirection: "column" as const }
      return <div className={styles.view_root} key={tree.id.toString()} style={style}>
               <div key={tree.id.toString() + "l"}
                 style={left_style}>{buildTree(tree.left)}</div>
               <div className={styles.handle + " " + styles[tree.direction]}
                 onPointerDown={(event) => onPointerDown(tree.direction, tree.id, event)}
                 onPointerUp={(event) => onPointerUp(tree.id, event)}>
               </div>
               <div key={tree.id.toString() + "r"}
                 style={right_style}>{buildTree(tree.right)}</div>
             </div>
    }
  }

  return buildTree(props.model.tree);
}

export interface ViewProps {
  children: React.ReactElement;
  id: string;
  label?: string;
}

export function View(props: ViewProps): JSX.Element {
  return props.children;
}

interface TabBarProps {
  onSplitHorz: () => void;
  onSplitVert: () => void;
  onClose: () => void;
  onTabSelected: (id: string) => void;
  onContextMenu: (clientX: number, clientY: number, id: string) => void;
  onDrop: (clientX: number, clientY: number, id: string) => void;
  onShouldUpdateDragPreview: (clientX: number, clientY: number, id: string) => void;
  records: { label: string | undefined, id: string }[];
  selectedTabId: string | null;
}

function TabBar(props: TabBarProps) {
  let tabs: JSX.Element[] =
    props.records.map(record =>
      <TabBarTab
        onShouldUpdateDragPreview={(clientX, clientY) => props.onShouldUpdateDragPreview(clientX, clientY, record.id)}
        onDrop={(clientX, clientY)=> props.onDrop(clientX, clientY, record.id)}
        onClick={() => props.onTabSelected(record.id)}
        key={record.id.toString()}
        label={record.label ?? "tab " + record.id.toString()}
        isSelected={record.id === props.selectedTabId}
        onContextMenu={(clientX, clientY) => props.onContextMenu(clientX, clientY, record.id)}/>);
  let withSpacers: JSX.Element[] = [];
  for(let i = 0; i < tabs.length; ++i) {
    withSpacers.push(tabs[i]!);
    if(i !== tabs.length - 1) {
      withSpacers.push(<span key={props.records[i]!.id + ".spacer"} className={styles.tab_spacer}></span>);
    }
  }
  return <div className={styles.tab_bar_root + " " + styles.absolute_tab_bar_root}>
           { props.records.length > 0 ?
             <div className={styles.tab_bar_group}>
               <span className={styles.tab_bar_left_endcap}></span>
               {withSpacers}
               <span className={styles.tab_bar_right_endcap}></span>
             </div> : <></> }
           <div className={styles.tab_bar_group}>
             <span className={styles.tab_bar_left_endcap}></span>
             { props.records.length === 0 ?
               <SplitViewButtonCluster
                 onSplitHorz={props.onSplitHorz}
                 onSplitVert={props.onSplitVert}
                 onClose={props.onClose}/> :
               <SplitViewButtonCluster
                 onSplitHorz={props.onSplitHorz}
                 onSplitVert={props.onSplitVert}/> }
             <span className={styles.tab_bar_right_endcap}></span>
           </div>
         </div>
}

interface TabBarTabProps {
  label: string;
  onContextMenu: (clientX: number, clientY: number) => void;
  onClick: () => void;
  onShouldUpdateDragPreview: (clientX: number, clientY: number) => void;
  onDrop: (clientX: number, clientY: number) => void;
  isSelected: boolean;
}

type DragState = {
  initialX: number;
  initialY: number;
  x: number;
  y: number;
  topLeftOffsetX: number;
  topLeftOffsetY: number;
}

function dragDistance(drag: DragState) {
  return Math.sqrt(Math.pow(drag.initialX - drag.x, 2) + Math.pow(drag.initialY - drag.y, 2));
}

function TabBarTab(props: TabBarTabProps): JSX.Element {
  const onPointerMove = useRef<((event: PointerEvent) => void) | null>(null);
  const [dragState, setDragState] = useState<DragState | null>(null);

  function onPointerDown(event: React.PointerEvent) {
    if(event.button !== 0) {
      /* Don't accept event if not left click */
      return;
    }
    event.preventDefault();

    event.currentTarget.setPointerCapture(event.pointerId);
    const rect = (event.currentTarget as HTMLDivElement).getBoundingClientRect();

    onPointerMove.current = (event: PointerEvent) => {
      props.onShouldUpdateDragPreview(event.clientX, event.clientY);
      setDragState(oldState => {
        if(oldState === null) {
          return {
            initialX: event.clientX,
            initialY: event.clientY,
            x: event.clientX,
            y: event.clientY,
            topLeftOffsetX: event.clientX - rect.left,
            topLeftOffsetY: event.clientY - rect.top
          }
        }
        return { ...oldState, x: event.clientX, y: event.clientY };
      });
    }
    (event.currentTarget as HTMLDivElement).onpointermove = onPointerMove.current;
  }

  function onPointerUp(event: React.PointerEvent) {
    if(event.button !== 0) {
      /* Don't accept event if not left click */
      return;
    }
    event.preventDefault();

    event.currentTarget.releasePointerCapture(event.pointerId);
    (event.currentTarget as HTMLSpanElement).onpointermove = null;
    onPointerMove.current = null;

    if(dragState === null || dragDistance(dragState) < 3) {
      /* onPointerMove never fired, or the drag was less than three pixels  */
      props.onClick();
    } else {
      props.onDrop(dragState.x, dragState.y);
    }
    setDragState(null);
  }

  function onContextMenu(event: React.MouseEvent) {
    event.stopPropagation();
    event.preventDefault();
    const rect = (event.target as HTMLSpanElement).getBoundingClientRect();
    props.onContextMenu(rect.left, rect.bottom);
  }

  const style = dragState !== null ? { display: "none" } : {};

  return <>
           <div style={style} className={styles.tab_bar_tab + (props.isSelected ? " " + styles.tab_bar_tab_selected : "")}>
             <span onPointerDown={onPointerDown} onPointerUp={onPointerUp} onContextMenu={onContextMenu}>
               {props.label}
             </span>
           </div>
           {dragState !== null ?
             <FloatingComponent x={dragState.x - dragState.topLeftOffsetX} y={dragState.y - dragState.topLeftOffsetY}>
               <div className={styles.tab_bar_root}>
                 <div className={styles.tab_bar_group}>
                   <span className={styles.tab_bar_left_endcap}></span>
                   <div className={styles.tab_bar_tab}>
                     <span style={{whiteSpace: "nowrap"}}
                       onPointerDown={onPointerDown}
                       onPointerUp={onPointerUp}>
                       {props.label}
                     </span>
                   </div>
                   <span className={styles.tab_bar_right_endcap}></span>
                 </div>
               </div>
             </FloatingComponent> : <></> }
         </>
}

interface SplitViewButtonClusterProps {
  onSplitHorz: () => void;
  onSplitVert: () => void;
  onClose?: () => void;
}

function SplitViewButtonCluster(props: SplitViewButtonClusterProps): JSX.Element {
  return <div className={styles.button_cluster_root}>
           <img className={styles.icon}
             src={require('./static/split-view-horizontal.svg')} onClick={props.onSplitHorz}/>
           <img className={styles.icon}
             src={require('./static/split-view-vertical.svg')} onClick={props.onSplitVert}/>
           { props.onClose ?
             <img className={styles.icon}
               src={require('./static/close-view.svg')} onClick={props.onClose}/> : <></> }
         </div>
}

interface SizeInformerProps {
  onSize: (rect: DOMRect) => void;
  children: React.ReactElement | undefined;
}

function SizeInformer(props: SizeInformerProps): JSX.Element {
  const divRef = useRef<HTMLDivElement>(null);
  React.useLayoutEffect(() => {
    if(divRef.current === null) return;
    props.onSize(divRef.current.getBoundingClientRect());
  });

  return <div style={{ width: "100%", height: "100%"}}
           ref={divRef}>{props.children}</div>;
}

function clamp(v: number, min: number, max: number): number {
  if(v < min) return min;
  if(v > max) return max;
  return v;
}
