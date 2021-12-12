# frozen_string_literal: true

# Methdos for Day 11. Simulating flashes of Dumbo Octopi on the ocean floor.
module DumboOctopus
  def self.parse_input(io)
    octopus_map = iterate_over_input(io) { |point, energy| Octopus.new(point, energy) }

    # It may be possible to populate the neighbor arrays on the first pass,
    # but I find this clearer and since the input is limited to 100 items
    # I'm not too concerned about doing two passes.
    octopus_map.each do |octopus_row|
      octopus_row.each do |octopus|
        octopus.neighbors = neighboring_octopi(octopus_map, octopus.position)
      end
    end

    octopus_map.flatten
  end

  def self.iterate_over_input(io)
    io.each_line.map.with_index do |line, row|
      line.chomp.chars.map.with_index do |energy_level, column|
        yield [[row, column], energy_level.to_i]
      end
    end
  end

  def self.draw_energy_level_map(octopi)
    lines = []
    octopi.each do |octopus|
      row, column = octopus.position
      lines[row] ||= ''
      lines[row] = lines[row].ljust(column, ' ')
      lines[row][column] = octopus.energy_level.to_s
    end
    lines.join("\n")
  end

  def self.neighboring_points(x, y)
    [
      [x - 1, y + 1],
      [x, y + 1],
      [x + 1, y + 1],
      [x - 1, y],
      [x + 1, y],
      [x - 1, y - 1],
      [x, y - 1],
      [x + 1, y - 1],
    ].select { |(a, b)| a >= 0 && b >= 0 }
  end

  def self.neighboring_octopi(octopus_map, point)
    neighboring_points(point[0], point[1]).filter_map do |(x, y)|
      octopus_map.dig(x, y)
    end
  end

  # Returns the number of flashes that happened in the step
  def self.simulate_step(octopi, step_id = generate_step_nonce)
    octopi.sum do |octopus|
      octopus.increment_energy(step_id: step_id)
    end
  end

  # Returns the number of flashes that happened in the step
  def self.simulate(octopi)
    Enumerator.produce { simulate_step(octopi) }
  end

  def self.find_bright_step(octopi)
    octopus_count = octopi.length
    simulate(octopi).each_with_index do |flash_count, step_number|
      return step_number + 1 if flash_count == octopus_count
    end
  end

  def self.generate_step_nonce
    rand
  end

  # Models a single octopus on the ocean floor. It will flash when its energy
  # reaches 9, resetting its energy to 0. The flash will cause its neighbors'
  # energy levels to increase. When an octopus flashes, its energy is reset to
  # 0 and it cannot gain any more energy until the next turn.
  class Octopus
    attr_accessor :neighbors
    attr_reader :position, :energy_level, :last_step_flashed

    def initialize(position, energy_level, neighbors = [])
      self.position = position
      self.energy_level = energy_level
      self.neighbors = neighbors
    end

    def increment_energy(source: nil, step_id: nil)
      return 0 unless should_increment?(source, step_id)

      if energy_level >= 9
        self.energy_level = 0
        self.last_step_flashed = step_id
        1 + neighbors.sum { |other| other.increment_energy(source: self, step_id: step_id) }
      else
        self.energy_level += 1
        0
      end
    end

    def should_increment?(source, step_id)
      # Don't increment (i.e. return false) if the source was our own flash or
      # if we have already flashed during this step. Otherwise, return true.
      source != self && (step_id.nil? || step_id != last_step_flashed)
    end

    # Since `neighbors` is recursive, `Object#inspect` will end up trying to
    # generate an infinite string. We need override it to avoid that.
    def inspect
      "<#{self.class} @position=#{position} @energy_level=#{energy_level} @neighbors=#{neighbors.map(&:position)}>"
    end

    private

    attr_writer :position, :energy_level, :last_step_flashed
  end
end
