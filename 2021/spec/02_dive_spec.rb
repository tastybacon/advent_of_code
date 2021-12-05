# frozen_string_literal: true

require 'dive'

describe 'Day 02: Dive!' do
  context 'Part 1' do
    it 'follows the test instructions correctly' do
      instruction_input = File.read('resources/02/test_instructions.txt')
      parsed_instructions = Dive.parse_instructions(instruction_input)
      instruction_executor = Dive::SimpleInstructionExecutor.new
      instruction_executor.follow_instructions(parsed_instructions)

      horizontal_position = instruction_executor.horizontal_position
      depth = instruction_executor.depth

      expect(horizontal_position).to eq 15
      expect(depth).to eq 10
      expect(horizontal_position * depth).to eq 150
    end

    it 'follows the actual instructions' do
      instruction_input = File.read('resources/02/instructions.txt')
      parsed_instructions = Dive.parse_instructions(instruction_input)
      instruction_executor = Dive::SimpleInstructionExecutor.new
      instruction_executor.follow_instructions(parsed_instructions)

      horizontal_position = instruction_executor.horizontal_position
      depth = instruction_executor.depth

      expect(horizontal_position * depth).to eq 1_746_616
    end
  end

  context 'Part 2' do
    it 'follows the test instructions correctly' do
      instruction_input = File.read('resources/02/test_instructions.txt')
      parsed_instructions = Dive.parse_instructions(instruction_input)
      instruction_executor = Dive::AimBasedInstructionExecutor.new
      instruction_executor.follow_instructions(parsed_instructions)

      horizontal_position = instruction_executor.horizontal_position
      depth = instruction_executor.depth

      expect(horizontal_position).to eq 15
      expect(depth).to eq 60
      expect(horizontal_position * depth).to eq 900
    end

    it 'follows the actual instructions' do
      instruction_input = File.read('resources/02/instructions.txt')
      parsed_instructions = Dive.parse_instructions(instruction_input)
      instruction_executor = Dive::AimBasedInstructionExecutor.new
      instruction_executor.follow_instructions(parsed_instructions)

      horizontal_position = instruction_executor.horizontal_position
      depth = instruction_executor.depth

      expect(horizontal_position * depth).to eq 1_741_971_043
    end
  end
end
