##
# Factory helper for creating and deleting wallets and keeping their ids in shared context.
# Assumes CONTEXT, SHELLEY, SHARED, BYRON to be initialized.
class WalletFactory
  def self.create(type, payload)
    case type
    when :shelley
      wallet = SHELLEY.wallets.create(payload)
      CONTEXT.shelley << wallet['id']
      wallet
    when :shared
      wallet = SHARED.wallets.create(payload)
      CONTEXT.shared << wallet['id']
      wallet
    when :byron
      wallet = BYRON.wallets.create(payload)
      CONTEXT.byron << wallet['id']
      wallet
    else
      raise "Unsupported wallet type: #{type}."
    end
  end

  def self.delete(type, wid)
    case type
    when :shelley
      CONTEXT.shelley.delete(wid)
      SHELLEY.wallets.delete(wid)
    when :shared
      CONTEXT.shared.delete(wid)
      SHARED.wallets.delete(wid)
    when :byron
      CONTEXT.byron.delete(wid)
      BYRON.wallets.delete(wid)
    else
      raise "Unsupported wallet type: #{type}."
    end
  end
end
