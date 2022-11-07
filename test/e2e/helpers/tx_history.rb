# frozen_string_literal: true

# helper methods for inspecting tx history from GET/LIST tx or decoded tx
module TxHistory
  def tx_inputs(tx, present: true)
    if present
      expect(tx['inputs']).not_to eq []
    else
      expect(tx['inputs']).to eq []
    end
  end

  def tx_outputs(tx, present: true)
    if present
      expect(tx['outputs']).not_to eq []
    else
      expect(tx['outputs']).to eq []
    end
  end

  def tx_direction(tx, direction)
    expect(tx['direction']).to eq direction
  end

  def tx_status(tx, status)
    expect(tx['status']).to eq status
  end

  def tx_script_validity(tx, validity)
    expect(tx['script_validity']).to eq validity
  end

  def tx_script_integrity(tx, present: true)
    if present
      expect(tx['script_integrity']).to start_with 'script_data'
    else
      expect(tx['outputs']).to eq []
    end
  end

  def tx_extra_signatures(tx, present: true)
    if present
      tx['extra_signatures'].each do |e|
        expect(e).to start_with 'req_signer_vkh'
      end
    else
      expect(tx['extra_signatures']).to eq nil
    end
  end

  def tx_collateral(tx, present: true)
    if present
      expect(tx['collateral']).not_to eq []
    else
      expect(tx['collateral']).to eq []
    end
  end

  def tx_collateral_outputs(tx, present: true)
    if present
      expect(tx['collateral_outputs']).not_to eq []
    else
      expect(tx['collateral_outputs']).to eq []
    end
  end

  def tx_metadata(tx, metadata)
    expect(tx['metadata']).to eq metadata
  end

  def tx_deposits(tx, deposit_taken: taken_amt, deposit_returned: returned_amt)
    expect(tx['deposit_taken']).to eq({ 'quantity' => deposit_taken, 'unit' => 'lovelace' })
    expect(tx['deposit_returned']).to eq({ 'quantity' => deposit_returned, 'unit' => 'lovelace' })
  end

  def tx_withdrawals(tx, present: true)
    if present
      expect(tx['withdrawals']).not_to eq []
    else
      expect(tx['withdrawals']).to eq []
    end
  end

  def tx_mint_burn(tx, mint: mint_tokens, burn: burn_tokens)
    expect(tx['mint']['tokens']).to eq mint
    expect(tx['burn']['tokens']).to eq burn
  end
end
