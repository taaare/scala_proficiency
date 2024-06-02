//define case classes
case class Rate(rateCode: String, rateGroup: String)
case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)
case class BestGroupPrice(cabinCode: String, rateCode: String, price: BigDecimal, rateGroup: String)

object CruisePricing {
  def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {
    //map ratecode to rategroup
    val rateMap = rates.map(rate => rate.rateCode -> rate.rateGroup).toMap

    //group prices accordingly
    val groupedPrices = prices
      .filter(price => rateMap.contains(price.rateCode)) //filter to ensure existence
      .groupBy(price => rateMap(price.rateCode))

      //find best prices
      groupedPrices.flatMap { case (rateGroup, cabinPrices) =>
      cabinPrices.groupBy(_.cabinCode).map { case (cabinCode, pricesForCabin) =>
        val bestPrice = pricesForCabin.minBy(_.price)
        BestGroupPrice(bestPrice.cabinCode, bestPrice.rateCode, bestPrice.price, rateGroup)
      }
    }.toSeq
  }

  //main method
  def main(args: Array[String]): Unit = {
    val rates = Seq(
      Rate("MilAB", "Military"),
      Rate("Sen123", "Senior"),
      Rate("Std001", "Standard")
    )

    val prices = Seq(
      CabinPrice("Cabin1", "MilAB", 100.00),
      CabinPrice("Cabin1", "Sen123", 80.00),
      CabinPrice("Cabin1", "Std001", 120.00),
      CabinPrice("Cabin2", "MilAB", 90.00),
      CabinPrice("Cabin2", "Sen123", 70.00),
      CabinPrice("Cabin2", "Std001", 110.00)
    )

    val bestPrices = getBestGroupPrices(rates, prices)
    bestPrices.foreach(println)
  }
}
