package hub

import hub.DSL._

/**
  * Created by guille on 18/12/2018.
  */

object Examples {

  val u1 = ("x" := "y") & ("c" := "z") | ("i" := 0)
  val u2 = ("b" := "i") & ("z" := 1) | ("a" := 2)

  val u3 = ("b" := "i") & (("z" := 1) | ("a" := 2))

  val u4 = ("b" := 1) & ("d":= "b")

  val u5 = ("a" := "x")

  val u6 = u5  | ("r" := "c")

  val u7 = ("b" := "x") & ("z" := 1)

  val u8 = ("z" := 1) & (u5 | u4)

  // example conflict

  val u9 = ("b" := "x") & ("y" := "o")
  val u10 = ("m" := "y") & ("x" := 1)

}