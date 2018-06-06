package com.davidron.scalaforjava

import org.junit._
import Assert._

class NumbersAreObjectsTest{
    
    val x = 4
    
    val looksLikeJava = 1 + 2 * 3 / x
    
    val theyAreActuallyObjects = (1).+(((2).*(3))./(x))
    
    @Test
    def testNumbersAreObject() =  assertEquals( looksLikeJava , theyAreActuallyObjects )
}

class FunctionsAreObjectTest{
    
    def thisFunctionReturnsAFunction() : String=>String = {
        string=>"This is a string: "+string
    }
    
    @Test
    def testNumbersAreObject() =  assertEquals( thisFunctionReturnsAFunction( )( "Ho!" ) , "This is a string: Ho!" )
}

