package com.davidron.scalaforjava

import org.junit._
import Assert._

class NumbersAreObjectsTest{
    
    val x = 4
    
    val looksLikeJava = 1 + 2 * 3 / x
    
    val theyAreActuallyObjects = (1).+(((2).*(3))./(x))
    
    @Test
    def testNumbersAreObject() =  
        assertEquals( looksLikeJava , theyAreActuallyObjects )
}

class FunctionsAreObjectsTest{
    
    def thisFunctionReturnsAFunction() : String=>String = {
        string=>"This is a string: "+string
    }
    
    @Test
    def testNumbersAreObject() =  
        assertEquals( thisFunctionReturnsAFunction( )( "Ho!" ) , "This is a string: Ho!" )
}

class ClassesTest {
    class TestClass(val publicField:String){
        def this() = this("auxiliary constructor")
    }
    
    @Test
    def testDefaultConstructor() =  
        assertEquals( new TestClass("constructor").publicField , "constructor" )
    
    @Test
    def testAuxiliaryConstructor() =  
        assertEquals( new TestClass().publicField , "auxiliary constructor" )
}

class CaseClassesAndPatternMatchingTest {
    abstract class Notification

    //Case classes are immutable. (variables public final)
    case class Email(sender: String, title: String, body: String) extends Notification

    case class SMS(caller: String, message: String) extends Notification

    case class VoiceRecording(contactName: String, link: String) extends Notification

    def getNotification(notification: Notification): String = {
      notification match {
        case Email(email, title, _) => 
          s"You got an email from $email with title: $title"
        case SMS(number, message) =>
          s"You got an SMS from $number! Message: $message"
        case VoiceRecording(name, link) =>
          s"you received a Voice Recording from $name! Click the link to hear it: $link"
      }
    }
    
    @Test
    def testCaseClassFieldsArePublicFinal() = 
        assertEquals( SMS("David", "Hello").caller , "David")
    
    @Test
    def testPatternMatching() =  
        assertEquals( getNotification(SMS(message="Hello", caller="David")) , "You got an SMS from David! Message: Hello" )
    
}

