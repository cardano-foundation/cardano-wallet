##
# cardano-address cmd helper wrapper
#
class CardanoAddresses
  ##
  # @param mnemonics
  def prv_key_from_recovery_phrase(mnemonics, cmd_params)
    cmd(%(echo #{mnemonics.join(' ')}| cardano-address key from-recovery-phrase #{cmd_params})).gsub("\n", '')
  end

  def key_public(key, with_chain_code = true)
    cmd(%(echo #{key}| cardano-address key public #{with_chain_code ? "--with-chain-code" : "--without-chain-code"})).gsub("\n", '')
  end

  def key_child(key, derivation_path)
    cmd(%(echo #{key}| cardano-address key child #{derivation_path})).gsub("\n", '')
  end

  def key_walletid(key, templates = '')
    cmd(%(echo #{key}| cardano-address key walletid #{templates})).gsub("\n", '')
  end

  def script_hash(script)
    cmd(%(cardano-address script hash "#{script}")).gsub("\n", '')
  end
end
