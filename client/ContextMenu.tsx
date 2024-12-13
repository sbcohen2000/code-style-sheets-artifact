import * as React from "react";
import { useState } from "react";
import styles from "./context-menu.module.css";
import FloatingComponent from "./FloatingComponent";

export type MenuTreeEntry = {
  type: "Entry";
  action: () => void;
  label: string;
  iconSrc?: string;
}

export type MenuTreeSubmenu = {
  type: "Submenu";
  items: MenuTree[];
  label: string;
}

export type MenuTree = MenuTreeEntry | MenuTreeSubmenu;

interface ContextMenuProps {
  x: number;
  y: number;
  menu: MenuTreeSubmenu;
  onShouldClose: () => void;
}

type PathSegment = {
  clientX: number;
  clientY: number;
  itemIndex: number;
}

export function ContextMenu(props: ContextMenuProps): JSX.Element {
  const [path, setPath] = useState<PathSegment[]>([]);

  function onItemClicked(event: React.MouseEvent, item: MenuTree, idx: number) {
    if(item.type === "Entry") {
      item.action();
      props.onShouldClose();
    } else {
      const rect = event.currentTarget.getBoundingClientRect()
      setPath(path => path.concat({
        clientX: rect.right,
        clientY: rect.top,
        itemIndex: idx
      }));
    }
  }

  function onShouldClose(pathIndex: number, itemIndex: number) {
    if(path.length >= 1
      && pathIndex === path.length - 1
      && path[path.length - 1]!.itemIndex === itemIndex) {
        setPath(path => path.slice(0, -1));
      } else if(path.length === 0) {
        props.onShouldClose();
      }
  }

  let itemLists: JSX.Element[] = [
    <ItemList
      key={"0"}
      x={props.x}
      y={props.y}
      items={props.menu.items}
      onItemClicked={onItemClicked}
      onShouldClose={() => onShouldClose(0, 0)}/>
  ];

  let currentMenu: MenuTree = props.menu;
  for(let i = 0; i < path.length; ++i) {
    const segment = path[i]!;
    currentMenu = (currentMenu as MenuTreeSubmenu).items[segment.itemIndex]!;
    itemLists.push(
      <ItemList
        key={i.toString() + 1}
        x={segment.clientX}
        y={segment.clientY}
        items={(currentMenu as MenuTreeSubmenu).items}
        onItemClicked={onItemClicked}
        onShouldClose={() => onShouldClose(i, segment.itemIndex)}/>);
  }

  return <>{itemLists}</>;
}

interface ItemListProps {
  x: number;
  y: number;
  items: MenuTree[];
  onItemClicked: (event: React.MouseEvent, item: MenuTree, idx: number) => void;
  onShouldClose: () => void;
}

export function ItemList(props: ItemListProps): JSX.Element {
  const menuItems: JSX.Element[] = props.items.map((item, idx) =>
    <MenuItem
      key={idx.toString()}
      label={item.label}
      iconSrc={item.type === "Entry" ? item.iconSrc : undefined}
      onClick={(event: React.MouseEvent) => props.onItemClicked(event, item, idx)}/>);
  return <FloatingComponent
           zIndex={10000}
           onShouldClose={props.onShouldClose}
           x={props.x}
           y={props.y}>
           <div className={styles.context_menu_frame}>
             { menuItems }
           </div>
         </FloatingComponent>;
}

interface MenuItemProps {
  label: string;
  iconSrc: string | undefined;
  onClick: (event: React.MouseEvent) => void;
}

export function MenuItem(props: MenuItemProps): JSX.Element {
  return <div className={styles.menu_item_container} onClick={props.onClick}>
           {
             props.iconSrc !== undefined
               ? <img src={props.iconSrc} className={styles.icon}/>
               : <div className={styles.icon_placeholder}></div>
           }
           <span className={styles.menu_item_label}>{props.label}</span>
         </div>;
}
