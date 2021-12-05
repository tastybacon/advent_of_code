# frozen_string_literal: true

# Takes submarine diagnostic information and extracts useful information
class DiagnosticInterpreter
  def self.calculate_gamma_rate(bit_counts)
    bit_counts
      .map { |count| count.max_by { |_, v| v }[0] }
      .reverse
      .join
      .to_i(2)
  end

  def self.count_bits(diagnostic_lines)
    diagnostic_lines.each_with_object([]) do |line, bit_counts|
      bits = line.chomp.reverse.chars
      bits.each.with_index do |value, index|
        bit_counts[index] ||= Hash.new(0)
        bit_counts[index][value] += 1
      end
    end
  end

  def initialize(data)
    @data = data
  end

  def gamma_rate
    return @gamma_rate if defined?(@gamma_rate)

    @gamma_rate = DiagnosticInterpreter.calculate_gamma_rate(bit_counts)
  end

  def epsilon_rate
    mask = (1 << bit_counts.length) - 1
    gamma_rate ^ mask
  end

  def power_consumption
    gamma_rate * epsilon_rate
  end

  def bit_counts
    return @bit_counts if defined?(@bit_counts)

    @bit_counts = DiagnosticInterpreter.count_bits(@data)
  end

  def oxygen_generator_rating
    return @oxygen_generator_rating if defined?(@oxygen_generator_rating)

    selected_value = filter_life_support_value do |bit_counts|
      bit_counts.max_by { |k, v| [v, k] }.first
    end
    @oxygen_generator_rating = selected_value.first.to_i(2)
  end

  def c02_scrubber_rating
    return @c02_scrubber_rating if defined?(@c02_scrubber_rating)

    selected_value = filter_life_support_value do |bit_counts|
      bit_counts.min_by { |k, v| [v, k] }.first
    end
    @c02_scrubber_rating = selected_value.first.to_i(2)
  end

  def life_support_rating
    oxygen_generator_rating * c02_scrubber_rating
  end

  private

  def filter_life_support_value
    selected = @data.dup
    bit_counts.length.times do |index|
      selected_bit_counts = DiagnosticInterpreter.count_bits(selected).reverse
      filter_key = yield selected_bit_counts[index]
      selected.select! { |value| value[index] == filter_key }
      raise if selected.length.zero?
      break if selected.length == 1
    end
    selected
  end
end
