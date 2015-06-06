package dzufferey.smtlib

import org.scalatest._

class DRealSuite extends FunSuite {

  val sampleModel1 = """Solution:
beam.dx : [-inf, 1015.625] = [0, 0]
beam.dy : [-inf, 1015.625] = [0, 0]
beam.dz : [ ENTIRE ] = [-162.9375, -153.875]
beam.q_a : [-inf, 1] = [1, 1]
beam.q_i : [-inf, 1] = [0, 0]
beam.q_j : [-inf, 1] = [0, 0]
beam.q_k : [-inf, 1] = [0, 0]
leftmotor.angle : [-inf, 3.141592653589794] = [-3.141592653589792, 3.141592653589794]
leftmotor.angle_dt : [ ENTIRE ] = [1, 1]
leftmotor.q_a : [-inf, 1] = [-1, 1]
leftmotor.q_i : [-inf, 1] = [-1, 1]
leftmotor.q_j : [-inf, 1] = [-1, 1]
leftmotor.q_k : [-inf, 1] = [-1, 1]
leftwheel.dx : [-inf, 1015.625] = [-64, -56]
leftwheel.dx_dt : [ ENTIRE ] = [ -INFTY ]
leftwheel.dy : [-inf, 1015.625] = [-64, -56]
leftwheel.dy_dt : [ ENTIRE ] = [ -INFTY ]
leftwheel.dz : [ ENTIRE ] = [-160, -150.9375]
leftwheel.q_a : [-inf, 1] = [-1, 1]
leftwheel.q_a_dt : [ ENTIRE ] = [ -INFTY ]
leftwheel.q_i : [-inf, 1] = [-1, 1]
leftwheel.q_i_dt : [ ENTIRE ] = [ -INFTY ]
leftwheel.q_j : [-inf, 1] = [-1, 1]
leftwheel.q_j_dt : [ ENTIRE ] = [ -INFTY ]
leftwheel.q_k : [-inf, 1] = [-1, 1]
leftwheel.q_k_dt : [ ENTIRE ] = [ -INFTY ]
rightmotor.angle : [-inf, 3.141592653589794] = [-3.141592653589792, 3.141592653589794]
rightmotor.angle_dt : [ ENTIRE ] = [1, 1]
rightmotor.q_a : [-inf, 1] = [-1, 1]
rightmotor.q_i : [-inf, 1] = [-1, 1]
rightmotor.q_j : [-inf, 1] = [-1, 1]
rightmotor.q_k : [-inf, 1] = [-1, 1]
rightwheel.dx : [-inf, 1015.625] = [-64, -56]
rightwheel.dx_dt : [ ENTIRE ] = [ -INFTY ]
rightwheel.dy : [-inf, 1015.625] = [-64, -56]
rightwheel.dy_dt : [ ENTIRE ] = [ -INFTY ]
rightwheel.dz : [ ENTIRE ] = [-160, -154.21875]
rightwheel.q_a : [-inf, 1] = [-1, 1]
rightwheel.q_a_dt : [ ENTIRE ] = [ -INFTY ]
rightwheel.q_i : [-inf, 1] = [-1, 1]
rightwheel.q_i_dt : [ ENTIRE ] = [ -INFTY ]
rightwheel.q_j : [-inf, 1] = [-1, 1]
rightwheel.q_j_dt : [ ENTIRE ] = [ -INFTY ]
rightwheel.q_k : [-inf, 1] = [-1, 1]
rightwheel.q_k_dt : [ ENTIRE ] = [ -INFTY ]
siminput0_input : [ ENTIRE ] = [1, 1]
siminput1_input : [ ENTIRE ] = [1, 1]
tail.dz : [ ENTIRE ] = [-150, -140.9375]
tail.q_a : [-inf, 1] = [-1, 1]
tail.q_i : [-inf, 1] = [-1, 1]
tail.q_j : [-inf, 1] = [-1, 1]
tail.q_k : [-inf, 1] = [-1, 1]
"""

  test("parsing model 1"){
    val model = DRealParser.parse(sampleModel1)
    assert(model.isDefined)
    //Console.println(model.get.mkString("\n"))
  }

  test("checking dReal model parsing") {
    val x = Variable("x").setType(Real)
    val form1 = Eq(x, DRealDecl.cos(x))
    val form2 = And(Eq(x, Literal(2.0)), Eq(x, Literal(1.0)))
    val solver3 = new DRealHackI(QF_NRA, "dReal", Array("--model"), None, true, false, None)
    solver3.push
    solver3.assert(form1)
    assert( solver3.checkSat match {
      case Sat(model) => model.isDefined
      case _ => false
    })
  }

}
