class DictNode(var key : String, var value : Double,
               var left : DictNode, var right : DictNode) {
    
  def this(key : String, value : Double) = 
    this(key, value, null, null)
      
  def insert(key : String, value : Double) : Unit = {
    var curr = this
      
    while (curr != null) {
      val cmp = key.compare(curr.key)
        
      if (cmp < 0) {
          if (curr.left == null) 
            curr.left = new DictNode(key, value)
          else
            curr = curr.left
      } else if (cmp > 0) {
          if (curr.right == null)
            curr.right = new DictNode(key, value)
          else
            curr = curr.right
      } else {
        curr.value = value
        return
      }
    }
  }
}

class Dictionary(var root : DictNode) {
  def this() = this(null)
  
  def add(key : String, value : Double) : Unit = {
    if (root == null) root = new DictNode(key, value)
    else root.insert(key, value)
  }
  
  def find(key : String) : Double = {
    var curr = root
    
    while (curr != null) {
      val cmp = key.compare(curr.key)
      
      if (cmp < 0) curr = curr.left
      else if (cmp > 0) curr = curr.right
      else return curr.value
    }
    
    Double.MinValue
  }
}