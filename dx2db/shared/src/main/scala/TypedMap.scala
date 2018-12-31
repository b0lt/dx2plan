package dx2db

trait StringableKey {
  def asStringKey: String
}

trait StringMap[K, V <: StringableKey] {
  def backing: Map[K, V]

  private lazy val stringKeys: Map[String, K] = {
    backing.map {
      case (key, value) => (value.asStringKey, key)
    }.toMap
  }

  def apply(stringKey: String) = backing(stringKeys(stringKey))
  def get(stringKey: String) = stringKeys.get(stringKey).flatMap(backing.get)
}

trait TypedMap[Derived, K, V] {
  def backing: Map[K, V]
  def construct(map: Map[K, V]): Derived

  def apply(key: K): V = backing(key)
  def get(key: K): Option[V] = backing.get(key)

  def values: Iterable[V] = backing.values

  def filter(f: (Tuple2[K, V]) => Boolean): Derived = construct(backing.filter(f).toMap)
  def filterKeys(f: K => Boolean): Derived = construct(backing.filterKeys(f))
  def filterValues(f: V => Boolean): Iterable[V] = backing.values.filter(f)
  def groupBy[T](f: V => T): Map[T, Iterable[V]] = backing.values.groupBy(f)
  def map[T](f: ((K, V)) => T): Iterable[T] = backing.map(f)
  def flatMap[T](f: ((K, V)) => Iterable[T]): Iterable[T] = backing.flatMap(f)
}
