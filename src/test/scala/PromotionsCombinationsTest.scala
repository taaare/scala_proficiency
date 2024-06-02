import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PromotionCombinationsTest extends AnyFlatSpec with Matchers {

  "allCombinablePromotions" should "return the expected combinations" in {
    val promotions = Seq(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
    )

    val expected = Seq(
      PromotionCombo(Seq("P1", "P2")),
      PromotionCombo(Seq("P1", "P4", "P5")),
      PromotionCombo(Seq("P2", "P3")),
      PromotionCombo(Seq("P3", "P4", "P5"))
    )

    val result = PromotionCombinations.allCombinablePromotions(promotions)
    result should contain theSameElementsAs expected
  }

  "combinablePromotions" should "return the expected combinations for P1" in {
    val promotions = Seq(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
    )

    val expected = Seq(
      PromotionCombo(Seq("P1", "P2")),
      PromotionCombo(Seq("P1", "P4", "P5"))
    )

    val result = PromotionCombinations.combinablePromotions("P1", promotions)
    result should contain theSameElementsAs expected
  }

  "combinablePromotions" should "return the expected combinations for P3" in {
    val promotions = Seq(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
    )

    val expected = Seq(
      PromotionCombo(Seq("P3", "P2")),
      PromotionCombo(Seq("P3", "P4", "P5"))
    )

    val result = PromotionCombinations.combinablePromotions("P3", promotions)
    result should contain theSameElementsAs expected
  }
}
