# frozen_string_literal: true

module PassagePathing
  # Represents one part of a cave system. It can be the start point, the end
  # point, a small cave, or a large cave.
  class Node
    def self.determine_node_type(label)
      case label
      when 'start'
        :start
      when 'end'
        :end
      when /^[a-z]+$/
        :small_cave
      when /^[A-Z]+$/
        :large_cave
      end
    end

    attr_reader :label, :type

    def initialize(label)
      @label = label
      @type = Node.determine_node_type(label)
    end
  end
end
