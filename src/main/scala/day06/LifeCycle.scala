package day06

enum LifeCycle:
  case _0, _1, _2, _3, _4, _5, _6, _7, _8

def previous(lifeCycle: LifeCycle): Option[LifeCycle] =
  Some(lifeCycle).collect {
    case LifeCycle._1 => LifeCycle._0
    case LifeCycle._2 => LifeCycle._1
    case LifeCycle._3 => LifeCycle._2
    case LifeCycle._4 => LifeCycle._3
    case LifeCycle._5 => LifeCycle._4
    case LifeCycle._6 => LifeCycle._5
    case LifeCycle._7 => LifeCycle._6
    case LifeCycle._8 => LifeCycle._7
  }
