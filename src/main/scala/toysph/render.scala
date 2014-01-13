package toysph

import java.awt.image.BufferedImage
import java.awt.{Color, RenderingHints}
import java.awt.geom.Ellipse2D
import java.io.File
import javax.imageio.ImageIO
import scala.util.Random

case class Bounds(x: Double, y: Double, width: Double, height: Double)

object ParticleBagRenderer {

  def renderToPNG(particleBag: ParticleBag, bounds: Bounds, pixelsPerUnit: Double,
                  file: File,
                  background: Color = Color.WHITE,
                  radiusFn: Particle2D => Double = (p: Particle2D) => 0.1,
                  colorFn: Particle2D => Color = (p: Particle2D) => Color.BLACK)
  {
    val img = render(particleBag, bounds, pixelsPerUnit, background, radiusFn, colorFn)
    ImageIO.write(img, "PNG", file)
  }

  def render(particleBag: ParticleBag, bounds: Bounds, pixelsPerUnit: Double,
             background: Color = Color.WHITE,
             radiusFn: Particle2D => Double = (p: Particle2D) => 0.1,
             colorFn: Particle2D => Color = (p: Particle2D) => Color.BLACK): BufferedImage = {

    // set up the image and graphics context
    val imgWidth = (bounds.width * pixelsPerUnit).toInt
    val imgHeight = (bounds.height * pixelsPerUnit).toInt
    val img = new BufferedImage(imgWidth, imgHeight, BufferedImage.TYPE_INT_RGB)
    val g = img.createGraphics()
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setColor(background)
    g.fillRect(0, 0, imgWidth, imgHeight)

    // render all the particles
    for {
      p <- particleBag.particles
      x = (p.r.x - bounds.x) * pixelsPerUnit
      y = (p.r.y - bounds.y) * pixelsPerUnit
      r = radiusFn(p) * pixelsPerUnit
      c = colorFn(p)
    } {
      g.setColor(c)
      g.fill(new Ellipse2D.Double(x - r, y - r, 2 * r, 2 * r))
    }

    // clean up the graphics context and return the image
    g.dispose()
    img

  }

}

// dumb test of the particle bag renderer
object ParticleBagRenderTest {

  def main(args: Array[String]) {

    // create a random bag of particles
    val pBagBuilder = ParticleBagBuilder(gridSize = 1.0)
    val random = new Random(42)
    for {
      i <- 0 until 10000
      x = random.nextDouble * 10
      y = random.nextDouble * 10
    } {
      val p = Particle2D(mass = 1.0, r = Vec2D(x, y), v = Vec2D(0, 0))
      pBagBuilder.add(p)
    }
    val pBag = pBagBuilder.result

    // render the particles to a test file
    val bounds = new Bounds(0, 0, 10, 10)
    ParticleBagRenderer.renderToPNG(pBag, bounds, 64.0, new File("test.png"),
      radiusFn = (p: Particle2D) => 0.02)

  }

}