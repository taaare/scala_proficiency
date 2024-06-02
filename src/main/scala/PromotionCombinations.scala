case class Promotion(code: String, notCombinableWith: Seq[String])
case class PromotionCombo(promotionCodes: Seq[String])

object PromotionCombinations {

  //find all combinable promotions with at least two combinable promotions in each combo
  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    val allCombinations = allPromotions.toSet.subsets().filter(isCombinable).toSeq

    //return combos with at least two
    val validCombinations = allCombinations.filter(_.size >= 2).map(p => PromotionCombo(p.map(_.code).toSeq.sorted))
    filterRedundantCombinations(validCombinations)
  }

  //checks if promo can be combined
  private def isCombinable(promotions: Set[Promotion]): Boolean = {
    val codes = promotions.map(_.code)
    promotions.forall { promo =>
      promo.notCombinableWith.forall(notWith => !codes.contains(notWith))
    }
  }

  //function to find promotions for given code
  def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    val targetPromotion = allPromotions.find(_.code == promotionCode).get
    val otherPromotions = allPromotions.filterNot(_.code == promotionCode)

    //generate promos that include target and can be combined
    val allCombinations = otherPromotions.toSet.subsets().filter(subset => isCombinable(subset + targetPromotion)).toSeq

    //return only valid combinations with two or more promotions
    val validCombinations = allCombinations.filter(_.size >= 1).map(p => PromotionCombo((p + targetPromotion).map(_.code).toSeq.sorted))
    filterRedundantCombinations(validCombinations)
  }

  //function to filter out redundant combinations
  private def filterRedundantCombinations(combinations: Seq[PromotionCombo]): Seq[PromotionCombo] = {
    combinations.filterNot { combo =>
      combinations.exists(other => combo != other && combo.promotionCodes.toSet.subsetOf(other.promotionCodes.toSet))
    }
  }

  def main(args: Array[String]): Unit = {
    val promotions = Seq(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
    )

    val allCombos = allCombinablePromotions(promotions)
    println("All Combinable Promotions:")
    allCombos.foreach(println)

    val p1Combos = combinablePromotions("P1", promotions)
    println("\nCombinable Promotions for P1:")
    p1Combos.foreach(println)

    val p3Combos = combinablePromotions("P3", promotions)
    println("\nCombinable Promotions for P3:")
    p3Combos.foreach(println)
  }
}
