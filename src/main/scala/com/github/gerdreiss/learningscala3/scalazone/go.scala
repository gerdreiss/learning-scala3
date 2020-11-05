package com.github.gerdreiss
package learningscala3
package rockthejvm

def go(x: Int): Int =
  if x == 0 then 1 else x * go(x/2)

@main def calculate() = println(go(24))