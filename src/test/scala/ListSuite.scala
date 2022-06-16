package me.evgeniychurikov.postmodern.fplab

import munit.FunSuite
import me.evgeniychurikov.postmodern.fplab

class ListSuite extends FunSuite {
  test("flatten on []") {
    val expected = List.Nil
    val actual = flatten(List.Nil)
    assertEquals(actual, expected)
  }
  test("flatten on [[1,2],[3,4],[5,6]]") {
    val expected = List(1,2,3,4,5,6)
    val actual = flatten(List(List(1,2),List(3,4),List(5,6)))
    assertEquals(actual, expected)
  }
  test("flatten on [[1,2],[],[3,4]") {
    val expected = List(1,2,3,4)
    val actual = flatten(List(List(1,2),List.Nil,List(3,4)))
    assertEquals(actual, expected)
  }
  test("intersperse on [], 0") {
    val expected = List.Nil
    val actual = intersperse(List.Nil, 0)
    assertEquals(actual, expected)
  }
  test("intersperse on [1], 0") {
    val expected = List(1)
    val actual = intersperse(List(1), 0)
    assertEquals(actual, expected)
  }
  test("intersperse on [1,2,3], 0") {
    val expected = List(1,0,2,0,3)
    val actual = intersperse(List(1,2,3), 0)
    assertEquals(actual, expected)
  }
  test("Intercalate on [], [[1], [2], [3]]") {
    val expected = List(1,2,3)
    val actual = intercalate(List.Nil, List(List(1),List(2),List(3)))
    assertEquals(actual, expected)
  }
  test("Intercalate on [], [[1,2], [3,4], [5,6]]") {
    val expected = List(1,2,3,4,5,6)
    val actual = intercalate(List.Nil, List(List(1,2),List(3,4),List(5,6)))
    assertEquals(actual, expected)
  }
  test("intercalate on [0], []") {
    val expected = List.Nil
    val actual = intercalate(List(0), List.Nil)
    assertEquals(actual, expected)
  }
  test("intercalate on [0], [[1], [2], [3]]") {
    val expected = List(1,0,2,0,3)
    val actual = intercalate(List(0), List(List(1),List(2),List(3)))
    assertEquals(actual, expected)
  }
  test("intercalate on [-1,0], [[1,2], [3,4], [5,6]]") {
    val expected = List(1,2,-1,0,3,4,-1,0,5,6)
    val actual = intercalate(List(-1,0), List(List(1,2),List(3,4),List(5,6)))
    assertEquals(actual, expected)
  }
  test("fromstring on \"\"") {
    val expected = List.Nil
    val actual = fromString("")
    assertEquals(actual, expected)
  }
  test("fromstring on \"Hello\"") {
    val expected = List('H','e','l','l','o')
    val actual = fromString("Hello")
    assertEquals(actual, expected)
  }
  test("tostring on []") {
    val expected = ""
    val actual = fplab.toString(List.Nil)
    assertEquals(actual, expected)
  }
  test("tostring on ['H','e','l','l','o']") {
    val expected = "Hello"
    val actual = fplab.toString(List('H','e','l','l','o'))
    assertEquals(actual, expected)
  }
}
