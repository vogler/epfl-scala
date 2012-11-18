package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CountChangeSuite extends FunSuite {
  import Main.countChange
  test("example from instructions") {
    assert(countChange(4,List(1,2)) === 3)
  }

  test("sorted CHF") {
    assert(countChange(300,List(5,10,20,50,100,200,500)) === 1022)
  }

  test("no pennies") {
    assert(countChange(301,List(5,10,20,50,100,200,500)) === 0)
  }

  test("unsorted CHF") {
    assert(countChange(300,List(500,5,50,100,20,200,10)) === 1022)
  }
  
  
//  [Test Description] coins can be reused
//  [Observed Error] org.scalatest.exceptions.TestFailedException: 0 did not equal 2
//  	[exception was thrown] detailed error message in debug output section below
//  [Lost Points] 10
  
  test("duplicate coins") { // not it
	assert(countChange(300,List(5,5,10,20,50,50,100,200,500)) === 1022)
  }
  
  test("smallest coin useless") {
	assert(countChange(9, List(2,3)) === 2) // 333, 3222 
  }
}
