case class Promotion(code: String, notCombinableWith: Seq[String])
case class PromotionCombo(promotionCodes: Seq[String])

object PromotionCombinations {

  //function to find all combinable promotions with at least two combinable promotions in each combo
  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    val allCombinations = allPromotions.toSet.subsets().filter(isCombinable).toSeq

    //return only valid combinations with at least two promotions
    allCombinations.filter(_.size >= 2).map(p => PromotionCombo(p.map(_.code).toSeq.sorted))
  }

  //checks if a set of promotions is combinable
  private def isCombinable(promotions: Set[Promotion]): Boolean = {
    val codes = promotions.map(_.code)
    promotions.forall { promo =>
      promo.notCombinableWith.forall(notWith => !codes.contains(notWith))
    }
  }

  //function to find promos for a given code
  def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    val targetPromotion = allPromotions.find(_.code == promotionCode).get
    val otherPromotions = allPromotions.filterNot(_.code == promotionCode)

    //generate all combinations that include the target promotion and are combinable
    val allCombinations = otherPromotions.toSet.subsets().filter(subset => isCombinable(subset + targetPromotion)).toSeq

    //return two param combinations
    allCombinations.filter(_.size >= 1).map(p => PromotionCombo((p + targetPromotion).map(_.code).toSeq.sorted))
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
