package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    for {
      a <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(a, h)
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  /** Якщо ви вставляєте будь-які два елементи в порожню
   * купу, знаходження мінімуму результуючої купи
   * повинно повернути найменший з двох елементів */
  property("insertTwoElementsInEmptyHeap") = {
    forAll { (a: Int, b: Int) =>
      val h = insert(b, insert(a, empty))
      val smallest = if (a < b) a else b
      findMin(h) == smallest
    }
  }

  /** Якщо ви вставляєте елемент у порожню купу,
   * а потім видаляєте мінімальний елемент,
   * то отримана купа повинна бути порожньою. */
  property("insertAndDeleteMinimal") = {
    forAll { a: Int =>
      val h = insert(a, empty)
      isEmpty(deleteMin(h))
    }
  }

  /** Для будь-якої купи, ви повинні отримувати відсортовану послідовність
   * елементів при постійному пошуку та видаленні мінімумів.
   * (Підказка: рекурсія та допоміжні функції - ваші друзі.) */
  property("findingAndRemovingMinimal") = {
    forAll { h: H =>
      def removeMin(current: H): List[Int] = {
        if (isEmpty(current)) Nil
        else findMin(current) +: removeMin(deleteMin(current))
      }

      val xs = removeMin(h)
      xs == xs.sorted
    }
  }

  /** Знаходження мінімуму злиття будь-яких двох куп
   * повинно повернути мінімум першої або другої купи. */
  property("findMeldingMinimum") = {
    forAll { (h1: H, h2: H) =>
      val min1 = findMin(h1)
      val min2 = findMin(h2)
      val newHeap = meld(h1, h2)
      val min = findMin(newHeap)
      min == min1 || min == min2
    }
  }

  /** Додавання та видалення двох елементів
   * до пустої купи повертає пусту купу */
  property("insertAndDeleteTwoElements") = {
    forAll { (a: Int, b: Int) =>
      val h = insert(b, insert(a, empty))
      val h1 = deleteMin(h)
      isEmpty(deleteMin(h1))
    }
  }

  /** Якщо додати елемент більше мінімального,
   * мінімальний елемент купи має залишитися без змін. */
//  property("insertGreaterThanMinimal") = {
//    forAll { h: H =>
//      findMin(insert(findMin(h) + 1, h)) == findMin(h)
//    }
//  }

  /** Якщо поєднати дві черги, потім видалити мінімум
   * першої, вставити в  другу, поєднати і перевірити
   * елементи черг, то вони мають бути однакові */
  property("compareTwoHeaps") = {
    forAll { (h1: H, h2: H) =>
      def remMin(ts: H, as: List[Int]): List[Int] = {
        if (isEmpty(ts)) as
        else findMin(ts) :: remMin(deleteMin(ts), as)
      }

      val meldHeap = meld(h1, h2)
      val newMeldHeap = meld(deleteMin(h1), insert(findMin(h1), h2))
      val new1 = remMin(meldHeap, Nil)
      val new2 = remMin(newMeldHeap, Nil)

      new1 == new2
    }
  }

  /** Список елементів поєднання черг має
   * не залежати від порядку поєднання */
//  property("heapOrderNotMatter") = {
//    forAll { (h1: H, h2: H) =>
//      meld(h1, h2) == meld(h2, h1)
//    }
//  }
}
