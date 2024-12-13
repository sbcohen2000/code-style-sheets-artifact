export function cssPropertyNameToJavascriptPropertyName(property: string) {
  return property.replace(/-+(.)/, (_match, firstLetter) =>
    firstLetter.toUpperCase());
}
