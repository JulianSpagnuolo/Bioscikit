pcr <- function(stock.fwd.prim, stock.rev.prim, stock.dNTP, stock.PCR.buffer, stock.DNApol, final.fwd.prim=0.2, final.rev.prim=0.2, final.dNTP, final.PCR.buffer = 1, final.DNApol, stock.template, final.template, rxn.vol=50)
{
  vol.fwd.prim <- dilution(c1=stock.fwd.prim, c2=final.fwd.prim, v2=rxn.vol)
  vol.rev.prim <- dilution(c1=stock.rev.prim, c2=final.rev.prim, v2=rxn.vol)
  vol.stock.dNTP <- dilution(c1=stock.dNTP, c2=final.dNTP, v2=rxn.vol)
  vol.PCR.buffer <- dilution(c1=stock.PCR.buffer, c2=final.PCR.buffer, v2=rxn.vol)
  vol.DNApol <- dilution(c1=stock.DNApol, c2=final.DNApol, v2=rxn.vol)
  vol.template <- dilution(c1=stock.template, c2=final.template, v2=rxn.vol)
  water <- rxn.vol-sum(vol.template, vol.DNApol, vol.PCR.buffer, vol.stock.dNTP, vol.rev.prim, vol.fwd.prim)

  pcr.setup <- data.frame(row.names=c("Fwd Primer", "Rev Primer", "dNTP", "PCR Buffer", "DNA polymerase", "Template", "Water", "Reaction Volume",Volume=c(vol.fwd.prim, vol.rev.prim, vol.stock.dNTP, vol.PCR.buffer, vol.DNApol, vol.template, water)))

  return(pcr.setup)
}
