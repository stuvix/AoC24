package general


class RangeList[T] extends Iterable[Element[T]] {
	private var first:Option[Element[T]] = None
	private var end:Option[Element[T]] = None
	/**
	 * number of elements in the list
	 */
	var length:Int = 0
	/**
	 * the size of all elements combined
	 */
	var blockLength:Int = 0

	def insert(start:Int, size:Int, payload:T):Unit = {
		if (length == 0) {
			val element = Element[T](None, None, start, size, payload)
			first = Some(element)
			end = Some(element)
			this.length = 1
		}
		else {
			val inserted:Element[T] = Element(None, None, start, size, payload)
			var before: Option[Element[T]] = None
			var after = first
			while (after.nonEmpty &&
				(if before.nonEmpty then before.get.start else Int.MinValue) <= start &&
				(if after.nonEmpty then after.get.start else Int.MaxValue) < start) {
				before = after
				after = after.get.next
			}

			inserted.prev = before

			inserted.next = after

			if after.isEmpty then end = Some(inserted) else after.get.prev = Some(inserted)

			if before.isEmpty then first = Some(inserted) else before.get.next = Some(inserted)

			this.length += 1
			mergeLeft(inserted)
			mergeRight(inserted)
		}
		blockLength += size
	}

	def findFirstOfMinSize(size:Int):Option[Element[T]] = {
		var currentOption = first
		var toReturn:Option[Element[T]] = None
		while (currentOption.nonEmpty && toReturn.isEmpty) {
			if (currentOption.get.size >= size) {
				toReturn = currentOption
			}
			else {
				currentOption = currentOption.get.next
			}
		}
		toReturn
	}

	def remove(element: Element[T]):Unit = {
		if (element.hasPrevious) {
			element.prev.get.next = element.next
		}
		else {
			this.first = element.next
		}
		if (element.hasNext) {
			element.next.get.prev = element.prev
		}
		else {
			this.end = element.prev
		}
		this.length -= 1
		blockLength -= element.size
	}

	private def mergeLeft(element: Element[T]):Unit = {
		if (element.hasPrevious) {
			val prev = element.prev.get
			if (prev.start + prev.size == element.start && element.payload == prev.payload) {
				prev.size += element.size
				prev.next = element.next
				if (end.get == element) {
					end = Some(prev)
				}
				length -= 1
			}
		}
	}

	private def mergeRight(element: Element[T]):Unit = {
		if (element.hasNext) {
			val next = element.next.get
			if (element.start + element.size == next.start && element.payload == next.payload) {
				element.size += next.size
				element.next = next.next
				if (end.get == next) {
					end = Some(element)
				}
				length -= 1
			}
		}
	}

	override def iterator: RangeListIt[T] = {
		val it = new RangeListIt[T]
		it.current = this.first
		it
	}

	override def last: Element[T] = end.get

	override def head: Element[T] = first.get

	def getForPayload(payload:T):Option[Element[T]] = {
		var ret:Option[Element[T]] = None
		for (element <- this) {
			if (element.payload == payload) {
				ret = Some(element)
			}
		}
		ret
	}
}

case class Element[T](var prev:Option[Element[T]], var next:Option[Element[T]], start:Int, var size:Int, payload:T) {
	def hasNext:Boolean = next.nonEmpty
	def hasPrevious:Boolean = prev.nonEmpty

	override def toString: String = f"start: $start size: $size -> $payload"
}

class RangeListIt[T] extends Iterator[Element[T]] {
	var current:Option[Element[T]] = None

	override def hasNext: Boolean = current.nonEmpty

	override def next(): Element[T] = {
		val ret = current.get
		current = current.get.next
		ret
	}
}