// Package declaration is similar to Java.  No ; needed.
package com.davidron.scalaforjava

// The "_" is a wildcard character
import org.junit._

// Since we imported org.junit._, Assert is available.
// Notice that static functions (Assert.assertEquals) are imported just like classes.
import Assert._

// Classes need not match their file name.
// This is considered bad form.
class NumbersAreObjectsTest{
    
    // The type is inferred to be a number
    val x = 4
    
    val looksLikeJava = 1 + 2 * 3 / x
    
    // This is what Scala is actually doing.  All of the paranthesis and dots are optional.
    // Notice that operators are just functions on their number objects.  
    val theyAreActuallyObjects = (1).+(((2).*(3))./(x))
    
    @Test
    def testNumbersAreObject() =  
        assertEquals( looksLikeJava , theyAreActuallyObjects )
}

// Multiple classes may exist in a file.
// This is considered bad form.
class StringsTest {
    val multiLine = """
        |This is a multi-line String.
        |Just use three-quotes.
        |""".stripMargin.replaceAll("\n", " ")
        
    val scalaString = s"Scala Strings allow embedded expressions ${5+1}"
        
    @Test
    def testMultiline() = 
        assertEquals(multiLine, " This is a multi-line String. Just use three-quotes. " )
        
    @Test
    def testScalaString() =  
        assertEquals(scalaString, "Scala Strings allow embedded expressions 6" )
}

class FunctionsAreObjectsTest{
    
    // Here we create a function who's return type is String=>String
    // In Java, it might look like:
    // Function<String, String> thisFunctionReturnsAFunction(){}
    def thisFunctionReturnsAFunction() : String=>String = {
        string=>"This is a string: "+string
    }
    
    @Test
    def testNumbersAreObject() =  
        assertEquals( thisFunctionReturnsAFunction( )( "Ho!" ) , "This is a string: Ho!" )
        // Isn't it nice not having to use .apply() ?
}

class OptionTest {
    
    // This is a type alias.  
    // In scala this can be useful for aliasing complex type signatures to make them easier to read
    // In Java this might be similar to an interface that exteds Optional<Function<String, String>>
    // Except in that case, we'd have a subtype rather than an alias
    type FunctionOption = Option[String=>String]
    
    // We create two optional functions.  One does something, the other is nothing.
    val optionalFunctionExists:FunctionOption = Option( input => s"Cool! ${input}" )
    val optionalFunctionNotExists:FunctionOption = None
    
    // This function will use "map" to apply the function if it exists.
    // Then we use getOrElse to return either the result of running that function or a fixed string.
    def applyOptionalFunction(optionalFunction:FunctionOption):String = 
        optionalFunction.map(f=>f("it works")).getOrElse("no function was found");
        
    // This is the same as the above function except we use pattern mathcing 
    // rather than map and getOrElse
    def applyOptionalPatterns(optionalFunction:FunctionOption):String = 
        optionalFunction match {
            case Some(f) => f("it works")
            case None => "no function was found"
        }
    
    @Test
    def testOptionFunctionGetsApplied() = 
        assertEquals( applyOptionalFunction( optionalFunctionExists ), "Cool! it works" ) 
        
    @Test
    def testEmptyFunctionNotApplied() = 
        assertEquals( applyOptionalFunction( optionalFunctionNotExists ), "no function was found" ) 
        
    @Test
    def testOptionFunctionGetsAppliedPatternMatching() = 
        assertEquals( applyOptionalPatterns( optionalFunctionExists ), "Cool! it works" ) 
        
    @Test
    def testEmptyFunctionNotAppliedPatternMatching() = 
        assertEquals( applyOptionalPatterns( optionalFunctionNotExists ), "no function was found" ) 
    
}

// Classes in Scala are very similar to Classes in Java.  
// But, "objects" take the place of statics
// If you are a java programmer, you may want to write code like this.
// If you are a scala programmer, you might consider these features of the language obscure
class ClassesTest {
    class TestClass(val publicField:String, private var privateField:String="defaultValue"){
        def this() = this("auxiliary constructor")
        def getPrivateField():String = privateField
    }
     
    object ObjectsAreJustStatics{
        def staticMethod():String = "Notice you don't have to call new"
    }
    
    object TestClass{
        def staticMethod():String = """By convention, objects with the same name as a class are called "Companion Objects"."""
    }

    @Test
    def testDefaultConstructor() =  
        assertEquals( new TestClass("constructor").publicField , "constructor" )
    
    @Test
    def testAuxiliaryConstructor() =  
        assertEquals( new TestClass().publicField , "auxiliary constructor" )
        
    @Test
    def testPrivateField() =  
        assertEquals( new TestClass().getPrivateField , "defaultValue" )
        
    @Test
    def testObjectsAreJustStatic() =  
        assertEquals(ObjectsAreJustStatics.staticMethod() , "Notice you don't have to call new" )
        
    @Test
    def testStaticMethod() =  
        assertEquals( TestClass.staticMethod() , """By convention, objects with the same name as a class are called "Companion Objects".""")
}

class ListsAreImmutableTest{

    // Any operator that end with a ":" is right-associative infix.
    // This means, operators that end with a ":" are functions on the RIGHT object
    // This is the opposite of operators such as "." which are applied to the LEFT object
    // As in lisp, Nil is an empty list.
    @Test def testLispLikeLists() =  
        assertEquals(List(1,2,3) , 1::2::3::Nil )
        
    @Test def testPrependingToLists() =  
        assertEquals(List(1,2,3) , 1+:List(2,3) )
    
    @Test def testAppendingToLists() =  
        assertEquals(List(1,2,3) , List(1,2):+3 )
        
    @Test def testTripleColonListMerge() =  
        assertEquals(List(1,2,3) , List(1,2):::List(3) )
        
    // Have a look at List https://www.scala-lang.org/api/2.12.3/scala/collection/immutable/List.html

}

class CaseClassesAndPatternMatchingTest {
    
    // Notice that Notification is nothing but a name.
    abstract class Notification

    //Case classes are immutable. (variables public final)
    case class Email(sender: String, title: String, body: String) extends Notification

    case class SMS(caller: String, message: String) extends Notification

    case class VoiceRecording(contactName: String, link: String) extends Notification

    // Scala requires us to enumerate every child of Notification here.
    // This is a popular way to attach behavior to data without encapsulating that behavior inside of an object with that data.
    // Case classes are extremely open for extension (as shown here) 
    //   and extremely closed for modification (immutable).
    //   See: https://en.wikipedia.org/wiki/Open/closed_principle
    // You can use "_" as a catch-all.
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
        
    @Test
    def testCaseClassesHaveAUsefulToString() = 
        assertEquals( SMS("David", "Hello").toString() , "SMS(David,Hello)" )

}

