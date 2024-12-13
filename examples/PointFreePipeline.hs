main =
  getContents
    >>= print
      . length
      . filter (not . isPrefixOf "--")
      . lines
