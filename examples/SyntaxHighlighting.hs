main =
  getContents
    >>= print
      . length
      . filter (isPrefixOf "--")
      . lines
