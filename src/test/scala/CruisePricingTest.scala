import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CruisePricingTest extends AnyFlatSpec with Matchers {
  import CruisePricing._

  "getBestGroupPrices" should "return the best prices for each rate group" in {
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

    val expected = Seq(
      BestGroupPrice("CA", "M1", 200.00, "Military"),
      BestGroupPrice("CA", "S1", 225.00, "Senior"),
      BestGroupPrice("CB", "M1", 230.00, "Military"),
      BestGroupPrice("CB", "S1", 245.00, "Senior")
    )

    val result = getBestGroupPrices(rates, prices)
    result should contain theSameElementsAs expected
  }

  it should "handle cases where there are no rates" in {
    val rates = Seq.empty[Rate]
    val prices = Seq(
      CabinPrice("CA", "M1", 200.00)
    )

    val expected = Seq.empty[BestGroupPrice]

    val result = getBestGroupPrices(rates, prices)
    result shouldEqual expected
  }

  it should "handle cases where there are no prices" in {
    val rates = Seq(
      Rate("M1", "Military")
    )
    val prices = Seq.empty[CabinPrice]

    val expected = Seq.empty[BestGroupPrice]

    val result = getBestGroupPrices(rates, prices)
    result shouldEqual expected
  }

  it should "handle cases where some rates have no corresponding prices" in {
    val rates = Seq(
      Rate("M1", "Military"),
      Rate("S1", "Senior")
    )
    val prices = Seq(
      CabinPrice("CA", "M1", 200.00)
    )

    val expected = Seq(
      BestGroupPrice("CA", "M1", 200.00, "Military")
    )

    val result = getBestGroupPrices(rates, prices)
    result shouldEqual expected
  }

  it should "handle cases where some prices have no corresponding rates" in {
    val rates = Seq(
      Rate("M1", "Military")
    )
    val prices = Seq(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "S1", 225.00)
    )

    val expected = Seq(
      BestGroupPrice("CA", "M1", 200.00, "Military")
    )

    val result = getBestGroupPrices(rates, prices)
    result shouldEqual expected
  }

  it should "handle cases where multiple cabins have prices for the same rate groups" in {
    val rates = Seq(
      Rate("M1", "Military"),
      Rate("S1", "Senior")
    )
    val prices = Seq(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CB", "S1", 245.00)
    )

    val expected = Seq(
      BestGroupPrice("CA", "M1", 200.00, "Military"),
      BestGroupPrice("CA", "S1", 225.00, "Senior"),
      BestGroupPrice("CB", "M1", 230.00, "Military"),
      BestGroupPrice("CB", "S1", 245.00, "Senior")
    )

    val result = getBestGroupPrices(rates, prices)
    result should contain theSameElementsAs expected
  }

  it should "handle cases with multiple prices for the same cabin and rate group" in {
    val rates = Seq(
      Rate("M1", "Military"),
      Rate("S1", "Senior")
    )
    val prices = Seq(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M1", 180.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S1", 215.00)
    )

    val expected = Seq(
      BestGroupPrice("CA", "M1", 180.00, "Military"),
      BestGroupPrice("CA", "S1", 215.00, "Senior")
    )

    val result = getBestGroupPrices(rates, prices)
    result should contain theSameElementsAs expected
  }
}
