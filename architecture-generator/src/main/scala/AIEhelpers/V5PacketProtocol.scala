package aiehelpers

/** In-band control record used to return one logical-input credit from AIE. */
object V5CreditProtocol {
  val TypeHeaderHigh32: BigInt = BigInt("FFFFC5ED", 16)
  val KernelTypeHeaderLiteral: String = "0xFFFFC5EDu"
}

