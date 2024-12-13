#!/usr/bin/env ruby
require 'pathname'

REPO_ROOT = `git rev-parse --show-toplevel`.chomp.freeze
DONT_ENTER = %w[
  dist-newstyle
  dist-client
  haskell-tree-sitter
  node_modules
].freeze

ALLOWED_EXTENSIONS = %w[
  .hs .tsx .ts .css
].freeze

@paths = []
def search
  Dir.each_child('.') do |entry|
    next if entry.start_with?('.')

    if File.directory?(entry) && !DONT_ENTER.include?(entry)
      Dir.chdir(entry) { search }
    elsif ALLOWED_EXTENSIONS.include?(File.extname(entry))
      @paths << File.expand_path(entry)
    end
  end
end

Dir.chdir(REPO_ROOT) { search }
@rel_paths = @paths.map do |p|
  Pathname.new(p).relative_path_from(Dir.pwd)
end
puts `wc -l #{@rel_paths.join(' ')}`
