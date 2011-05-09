package org.nlogo.headless

// Most testing of error messages can go in autogen/tests/commands.  Some tests are here because
// they go beyond the capabilities of the autogenerated test framework.  (In the long run, perhaps
// that framework should be extended so these tests could be done in it.)  - ST 3/18/08
import org.scalatest.{ FunSuite, BeforeAndAfterEach }
import org.nlogo.api.{ CompilerException, LogoException }
import org.nlogo.nvm.{ ArgumentTypeException, EngineException }

class TestErrorMessages extends AbstractTestLanguage with FunSuite with BeforeAndAfterEach {
  override def beforeEach() { init() }
  override def afterEach() { workspace.dispose() }
  test("perspectiveChangeWithOf") {
    testCommand("create-frogs 3 [ set spots turtle ((who + 1) mod count turtles) ]")
    testCommand("ask frog 2 [ die ]")
    val ex = intercept[EngineException] {
      testCommand("ask turtle 0 [ __ignore [who] of frogs with [age = ([age] of [spots] of self)]]")
    }
    // is the error message correct?
    expect("That turtle is dead.")(ex.getMessage)
    // is the error message attributed to the right agent? frog 2 is dead,
    // but it's frog 1 that actually encountered the error
    expect("frog 1")(ex.context.agent.toString)
  }
  test("argumentTypeException") {
    testCommand("set glob1 [1.4]")
    val ex = intercept[ArgumentTypeException] {
      testCommand("__ignore 0 < position 5 item 0 glob1")
    }
    expect("POSITION expected input to be a string or list but got the number 1.4 instead.")(ex.getMessage)
    expect("POSITION")(ex.instruction.token.name.toUpperCase)
  }
  test("breedOwnRedeclaration") {
    val ex = intercept[CompilerException] {
      compiler.compileProgram(
        "breed [hunters hunter] hunters-own [fear] hunters-own [loathing]",
        workspace.world.newProgram(java.util.Collections.emptyList[String]),
        workspace.getExtensionManager)
    }
    expect("Redeclaration of HUNTERS-OWN")(ex.getMessage)
  }

}
