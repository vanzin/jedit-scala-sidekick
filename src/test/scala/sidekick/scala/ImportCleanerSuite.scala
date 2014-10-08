//            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
//                    Version 2, December 2004
//
// Copyright (C) 2014 Marcelo Vanzin <vanza@users.sourceforge.net>
//
// Everyone is permitted to copy and distribute verbatim or modified
// copies of this license document, and changing it is allowed as long
// as the name is changed.
//
//            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
//   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
//
//  0. You just DO WHAT THE FUCK YOU WANT TO.
package sidekick.scala;

import org.gjt.sp.jedit.{Buffer, View}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.mockito.invocation._
import org.mockito.stubbing._
import org.scalatest.FlatSpec

class ImportCleanerSuite extends FlatSpec {

  "The Import Cleaner" should "clean a single unused import" in {
    val original = """
      |import com.example.Unused
      |
      |class Foo {
      |  val bar: Type = _
      |}
      |"""
    val expected = """
      |
      |class Foo {
      |  val bar: Type = _
      |}
      |"""
    runTest(original, expected)
  }

  it should "clean multiple unused imports" in {
    val original = """
      |import com.example.Unused
      |import com.example.Used
      |import com.example.Unused2
      |
      |class Foo {
      |  val bar: Used = _
      |}
      |"""
    val expected = """
      |import com.example.Used
      |
      |class Foo {
      |  val bar: Used = _
      |}
      |"""
    runTest(original, expected)
  }

  it should "clean imports from an import list" in {
    val original = """
      |import com.example.{Unused, Unused2, Used}
      |import com.example.foo.{Other, Other2, Other3}
      |
      |class Foo {
      |  val bar: Used = _
      |  val bar2: Other = _
      |  val bar3: Other2 = _
      |}
      |"""
    val expected = """
      |import com.example.Used
      |import com.example.foo.{Other, Other2}
      |
      |class Foo {
      |  val bar: Used = _
      |  val bar2: Other = _
      |  val bar3: Other2 = _
      |}
      |"""
    runTest(original, expected)
  }

  it should "clean renamed imports" in {
    val original = """
      |import com.example.{Unused, Unused2 => UnusedTwo, Used => ItsUsed}
      |import com.example.foo.{Other => Another, Other2, Other3 => UnusedThree}
      |
      |class Foo {
      |  val bar: ItsUsed = _
      |  val bar2: Another = _
      |  val bar3: Other2 = _
      |}
      |"""
    val expected = """
      |import com.example.{Used => ItsUsed}
      |import com.example.foo.{Other => Another, Other2}
      |
      |class Foo {
      |  val bar: ItsUsed = _
      |  val bar2: Another = _
      |  val bar3: Other2 = _
      |}
      |"""
    runTest(original, expected)
  }

  it should "clean non-top-level imports" in {
    val original = """
      |class Foo {
      |  import com.example._
      |  import com.example.foo.Unused1
      |  import com.example.bar.{Used, Unused}
      |
      |  val bar: Used = _
      |}
      |"""
    val expected = """
      |class Foo {
      |  import com.example._
      |  import com.example.bar.Used
      |
      |  val bar: Used = _
      |}
      |"""
    runTest(original, expected)
  }

  it should "handle static member access" in {
    val original = """
      |import com.example.Used
      |
      |class Foo {
      |  val bar = Used.FIELD
      |}
      |"""
    val expected = """
      |import com.example.Used
      |
      |class Foo {
      |  val bar = Used.FIELD
      |}
      |"""
    runTest(original, expected)
  }

  it should "ignore scala.language imports" in {
    val original = """
      |import scala.language
      |import scala.language.foo
      |
      |class Foo {
      |}
      |"""
    val expected = """
      |import scala.language
      |import scala.language.foo
      |
      |class Foo {
      |}
      |"""
    runTest(original, expected)
  }

  private def runTest(original: String, expected: String) = {
    val buffer = mockBuffer(original.stripMargin)
    val view = mockView(buffer)

    new ImportCleaner(view).run()
    assert(buffer.getText() === expected.stripMargin)
  }

  private def mockBuffer(text: String): Buffer = {
    val buffer = mock(classOf[Buffer])
    val sb = new StringBuilder(text)
    when(buffer.getText()).thenAnswer(
      new Answer[String]() {
        override def answer(call: InvocationOnMock) = sb.toString()
      })
    when(buffer.insert(anyInt(), any(classOf[String]))).thenAnswer(
      new Answer[Unit]() {
        override def answer(call: InvocationOnMock) = {
          val (offset, text) =
            (call.getArguments()(0).asInstanceOf[Int],
             call.getArguments()(1).asInstanceOf[String])
          sb.insert(offset, text)
        }
      })
    when(buffer.remove(anyInt(), anyInt())).thenAnswer(
      new Answer[Unit]() {
        override def answer(call: InvocationOnMock) = {
          val (start, len) =
            (call.getArguments()(0).asInstanceOf[Int],
             call.getArguments()(1).asInstanceOf[Int])
          sb.replace(start, start + len, "")
        }
      })
    buffer
  }

  private def mockView(buffer: Buffer): View = {
    val view = mock(classOf[View])
    when(view.getBuffer()).thenReturn(buffer)
    view
  }

}
