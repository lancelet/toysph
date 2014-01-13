package toysph

import org.scalatest.FunSpec
import scala.collection.breakOut
import scala.collection.immutable._
import scala.collection.mutable.ArrayBuilder
import toysph.ParticleBagBuilderSpec.SimpleParticleBagBuilder
import scala.util.Random

class ParticleBagBuilderSpec extends FunSpec {

  describe("ParticleBagBuilder") {

    it ("should allow construction and iteration over particles") {
      val p1 = Particle2D(1, 2.5, 2.5)
      val p2 = Particle2D(2, 1.5, 2.5)
      val p3 = Particle2D(3, 3.5, 2.5)
      val p4 = Particle2D(4, 2.5, 0.5)
      val ps = Seq(p1, p2, p3, p4)
      val pbb = ParticleBagBuilder(gridSize = 1.0)
      for (p <- ps) pbb.add(p)
      val prets = pbb.result.particles.toSeq
      for (p <- ps) assert(prets.contains(p))
    }

    it ("should correctly find neighbouring particles") {
      // construct particlebag
      val p1 = Particle2D(1, 2.5, 2.5)
      val p2 = Particle2D(2, 1.5, 2.5)
      val p3 = Particle2D(3, 3.5, 2.5)
      val p4 = Particle2D(4, 2.5, 0.5)
      val ps = Seq(p1, p2, p3, p4)
      val pbb = ParticleBagBuilder(gridSize = 1.0)
      for (p <- ps) pbb.add(p)
      val pb = pbb.result
      // check neighbours for different radii
      assert(pb.neighbours(p1, 0.5).isEmpty)
      assert(pb.neighbours(p1, 1.1).toSet === Set(p2, p3))
      assert(pb.neighbours(p1, 2.1).toSet === Set(p2, p3, p4))
    }

    it ("should behave the same as a naiive implementation") {
      // parameters (# of particles, spatial dimensions of test)
      val nParticles = 1000
      val maxValue = 10.0
      val testRadius = 1.0
      // create both a simple (naiive) implementaton and the real one
      val pbb = ParticleBagBuilder(gridSize = 1.0)
      val pbbSimple = new SimpleParticleBagBuilder
      // add random particles
      val random = new Random(42)
      for {
        i <- 0 until nParticles
        x = random.nextDouble * maxValue
        y = random.nextDouble * maxValue
        p = Particle2D(mass = 1.0, r = Vec2D(x, y), v = Vec2D(0, 0))
      } {
        pbb.add(p)
        pbbSimple.add(p)
      }
      // create particle bags
      val pb = pbb.result
      val pbSimple = pbbSimple.result
      // find neighbours for both types of particle bags and compare them
      for {
        p <- pbSimple.particles
        neigh = pb.neighbours(p, testRadius).toSet
        neighSimple = pbSimple.neighbours(p, testRadius).toSet
      } {
        assert(neigh === neighSimple)
      }
    }

  }

}

object ParticleBagBuilderSpec {

  // naiive / simple implementation of ParticleBagBuilder (used for comparison testing)
  private class SimpleParticleBagBuilder extends ParticleBagBuilder {
    private val particles = new ArrayBuilder.ofRef[Particle2D]
    def add(p: Particle2D): Unit = particles += p
    def result: ParticleBag = new SimpleParticleBag(particles.result)
  }

  // naiive / simple implementation of ParticleBag (used for comparison testing)
  private class SimpleParticleBag(private val partArray: Array[Particle2D]) extends ParticleBag {
    def particles: Iterable[Particle2D] = partArray.to[Iterable]
    def neighbours(a: Particle2D, radius: Double): Iterable[Particle2D] = {
      val d2 = radius * radius
      (for {
        p <- partArray
        if ((p != a) && (p.r.distance2To(a.r) < d2))
      } yield p)(breakOut)
    }
  }

}
