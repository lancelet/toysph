package toysph

import scala.collection.breakOut
import scala.collection.immutable._
import scala.collection.mutable.{ArrayBuilder, Map => MMap}
import scala.math._


trait Vec2D {
  def x: Double
  def y: Double
  def len2: Double = x * x + y * y
  def -(v: Vec2D): Vec2D = Vec2D(x - v.x, y - v.y)
  def distance2To(v: Vec2D): Double = (this - v).len2
}
object Vec2D {
  private case class FreeVec2D(x: Double, y: Double) extends Vec2D
  def apply(x: Double, y: Double): Vec2D = FreeVec2D(x, y)
}

trait Particle2D {
  def mass: Double  // mass
  def r: Vec2D      // position
  def v: Vec2D      // velocity
}
object Particle2D {
  private case class FreeParticle2D(mass: Double, r: Vec2D, v: Vec2D) extends Particle2D
  def apply(mass: Double, r: Vec2D, v: Vec2D): Particle2D = FreeParticle2D(mass, r, v)
  def apply(mass: Double, rx: Double, ry: Double, vx: Double = 0.0, vy: Double = 0.0): Particle2D =
    FreeParticle2D(mass, Vec2D(rx, ry), Vec2D(vx, vy))
}


trait ParticleBag {
  def particles: Iterable[Particle2D]
  def neighbours(p: Particle2D, radius: Double): Iterable[Particle2D]
}

trait ParticleBagBuilder {
  def add(p: Particle2D): Unit
  def result: ParticleBag
}


object ParticleBagBuilder {

  private case class Cell(row: Int, col: Int)
  private object Cell {
    def fromXY(x: Double, y: Double, gridSize: Double): Cell = Cell((x / gridSize).toInt, (y / gridSize).toInt)
    def forParticle(p: Particle2D, gridSize: Double): Cell = fromXY(p.r.x, p.r.y, gridSize)
  }

  private final class DefaultParticleBagBuilder(gridSize: Double) extends ParticleBagBuilder {
    def add(p: Particle2D): Unit = addToCell(Cell.forParticle(p, gridSize), p)
    def result: ParticleBag = new DefaultParticleBag(immutableMap, gridSize)
    private val gMap: MMap[Cell, ArrayBuilder[Particle2D]] = MMap.empty[Cell, ArrayBuilder[Particle2D]]
    private def addToCell(c: Cell, p: Particle2D): Unit = { ensureCellExists(c); gMap(c) += p }
    private def ensureCellExists(c: Cell): Unit = if (!gMap.contains(c)) gMap(c) = new ArrayBuilder.ofRef[Particle2D]
    private def immutableMap: Map[Cell, Array[Particle2D]] = gMap.map { case (c, ab) => (c, ab.result()) }(breakOut)
  }

  private final class DefaultParticleBag(gMap: Map[Cell, Array[Particle2D]], gridSize: Double) extends ParticleBag {
    def particles: Iterable[Particle2D] = gMap.values.flatMap(_.toIterable)(breakOut)
    def neighbours(p: Particle2D, radius: Double): Iterable[Particle2D] = {
      val nAdjCells = ceil(radius / gridSize).toInt
      val pCell = Cell.forParticle(p, gridSize)
      val r2 = radius * radius
      val cells: Iterable[Cell] = for {
        row <- (pCell.row - nAdjCells) to (pCell.row + nAdjCells)
        col <- (pCell.col - nAdjCells) to (pCell.col + nAdjCells)
        cell = Cell(row, col)
        if (gMap.contains(cell))
      } yield cell
      for {
        cell <- cells
        pb <- gMap(cell)
        if (pb != p)
        curDist2 = pb.r distance2To p.r
        if (curDist2 < r2)
      } yield pb
    }
  }

  def apply(gridSize: Double): ParticleBagBuilder = new DefaultParticleBagBuilder(gridSize)

}
