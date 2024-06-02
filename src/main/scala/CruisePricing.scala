case class Rate(rateCode: String, rateGroup: String)
case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)
case class BestGroupPrice(cabinCode: String, rateCode: String, price: BigDecimal, rateGroup: String)

object CruisePricing {
  //function to get the best group prices
  def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {
    //map rateCode to rateGroup
    val rateMap = rates.map(rate => rate.rateCode -> rate.rateGroup).toMap

    //group prices by cabinCode and rateGroup
    val groupedPrices = prices
      .filter(price => rateMap.contains(price.rateCode))
      .groupBy(_.cabinCode)
      .mapValues(_.groupBy(price => rateMap(price.rateCode)))

    //find the best price for each rate group
    groupedPrices.flatMap { case (cabinCode, pricesByGroup) =>
      pricesByGroup.map { case (rateGroup, pricesForGroup) =>
        val bestPrice = pricesForGroup.minBy(_.price)
        BestGroupPrice(bestPrice.cabinCode, bestPrice.rateCode, bestPrice.price, rateGroup)
      }
    }.toSeq
  }

  //main method for defs
  def main(args: Array[String]): Unit = {
    val rates = Seq(
      Rate("M1", "Military"),
      Rate("M2", "Military"),
      Rate("S1", "Senior"),
      Rate("S2", "Senior")
    )

    val prices = Seq(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 270.00)
    )

    val bestPrices = getBestGroupPrices(rates, prices)
    bestPrices.foreach(println)
  }
}
