# frozen_string_literal: true

##
# Class for keeping track of wallet ids. WalletFactory keeps track of the context.
#  CONTEXT = Context.new
#  WalletFactory.create(:shelley, payload)
#  CONTEXT.shelley.each do |wid|
#     WalletFactory.delete(:shelley, wid)
#  end
class Context
  attr_accessor :env, :shelley, :shared, :byron

  def initialize
    clear!
  end

  def clear!
    @shelley = []
    @shared = []
    @byron = []
  end
end
