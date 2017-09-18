import util.SimpleCache._

object SimpleCacheTest {
  type CustomType = (String, String)
  def splitter (a : String) : (String, CustomType) = 
    {
      val elems = a.split('|')
      return (elems(0), (elems(0), elems(1)))
    }
   def loadTest(aFile : String) = {
   		val cache : TMap[String, CustomType] = TMap()
   		cache.SimpleCache.loadPeriodically (cache) (splitter) (1000)

   }

}