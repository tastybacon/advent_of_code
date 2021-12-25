# frozen_string_literal: true

# Methods to solve day 14, counting the elements in a polymer after a given
# number of steps of polymerization.
module ExtendedPolymerization
  def self.parse_input(io)
    lines = io.each_line
    template = lines.first.chomp
    instructions = lines.drop(2).each_with_object({}) do |line, acc|
      pattern, addition = parse_instruction(line.chomp)
      acc[pattern[0]] ||= {}
      acc[pattern[0]][pattern[1]] = addition
    end
    { template: template, instructions: instructions }
  end

  def self.parse_instruction(line)
    line.split(' -> ')
  end

  def self.count_elements(template, instructions, steps)
    memoized_counts = {}
    template.each_cons(2).reduce(template.tally) do |acc, (a, b)|
      acc.merge!(rec(a, b, instructions, steps, memoized_counts)) { |_k, v1, v2| v1 + v2 }
    end
  end

  def self.rec(a, b, instructions, steps_left, memoized_counts)
    return {} if steps_left.zero?

    if memoized_counts.key?([steps_left, a, b])
      memoized_counts[[steps_left, a, b]]
    else
      new_letter = instructions.dig(a, b)
      count_a = rec(a, new_letter, instructions, steps_left - 1, memoized_counts)
      count_b = rec(new_letter, b, instructions, steps_left - 1, memoized_counts)
      result = { new_letter => 1 }.merge!(count_a, count_b) { |_k, v1, v2| v1 + v2 }
      memoized_counts[[steps_left, a, b]] = result
    end
  end

  # These are not actually used to solve the problem, but they were a part of my
  # initial approach and decided to leavee them in. They will output the actual
  # string representing the polymer. Seems to run in a reasonable amount of time
  # for the first 10 or 15 iterations.
  def self.run_instructions(template, instructions)
    Enumerator.produce(template.chars) { |prev| run_step(prev, instructions) }
  end

  def self.run_step(template, instructions)
    polymer = template.each_cons(2).flat_map { |(a, b)| [a, instructions.dig(a, b)] }
    polymer << template[-1]
    polymer
  end
end
