#!/usr/bin/env ruby

module Test
  def f
    1
  end

  def g
    f
  end

  p g
end
