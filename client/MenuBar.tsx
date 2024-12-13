import * as React from "react";
import { useState } from "react";
import styles from "./menu-bar.module.css";
import * as CM from "./ContextMenu";

interface MenuBarProps {
  menus: CM.MenuTreeSubmenu[];
}

type OpenMenuState = {
  menuIndex: number;
  clientX: number;
  clientY: number;
}

export function MenuBar(props: MenuBarProps): JSX.Element {
  const [openMenuState, setOpenMenuState] = useState<OpenMenuState | null>(null);
  function openMenu(event: React.MouseEvent, idx: number) {
    const rect = (event.target as HTMLSpanElement).getBoundingClientRect();
    setOpenMenuState({
      menuIndex: idx,
      clientX: rect.left,
      clientY: rect.bottom
    });
  }

  function onShouldClose() {
    setOpenMenuState(null);
  }

  const toplevel = props.menus.map((tree, idx) =>
    <span
      className={styles.toplevel_menu_item}
      onClick={(e) => openMenu(e, idx)}
      key={tree.label}>
      {tree.label}
    </span>
  );

  return <>
           <div className={styles.menu_bar_root}>{toplevel}</div>
           { openMenuState !== null ?
             <CM.ContextMenu
               x={openMenuState.clientX}
               y={openMenuState.clientY}
               menu={props.menus[openMenuState.menuIndex]!}
               onShouldClose={onShouldClose}/> : <></> }
         </>;
}
