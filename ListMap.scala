class ListMap[K, V] extends scala.collection.mutable.Map[K, V] {
  class Node[K, V](k: K, v: V) {
    val key = k
    val data = v
    var next: Node[K, V] = null
  }
  
  var root: Node[K, V] = null

  def get(key: K): Option[V] = {
    var current: Node[K, V] = root
    while (current != null) {
      if (current.key == key)
        return Some(current.data)
      current = current.next
    }
    None
  }

  def iterator: Iterator[(K, V)] = {
    val buf = new scala.collection.mutable.ListBuffer[(K,V)]()
    var current = root
    while (current != null) {
      buf += ((current.key, current.data))
      current = current.next
    }
    buf.toList.iterator
  }

  def += (kv: (K, V)) = {
    this -= kv._1
    val n = new Node(kv._1, kv._2)

    if (root == null)
      root = n
    else {
      var current = root
      while (current.next != null) {
        current = current.next
      }
      current.next = n
    }
    this
  }

  def -= (key: K) = {
    if (root != null) {
      if (root.key == key) {
        root = root.next
      }
      var prev = root
      var current = root.next
      while (current != null && current.key != key) {
        prev = current
        current = current.next
      }
      if (current != null && current.key == key) {
        prev.next = current.next
      }
    }
    this
  }
}

