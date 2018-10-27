actor Main
  new create(env: Env) =>
    env.out.print("Hello, World!")
    let w: Wombat = Wombat.createWithHunger("Foo", 10)
    let a: Aardvark = Aardvark.create("Bar")
    w.setHunger()
    a.eat(1).eat(1).eat(1)
    env.out.print("Wombat hunger: " + w.getHunger().string())
    // env.out.print("Aardvark hunger: " + a.getHungerLevel().string())

class Wombat
  let name: String
  var _hungerLevel: U8 = 0

  new create(name': String) =>
    name = name'

  new createWithHunger(name': String, hungerLevel': U8 = 1) =>
    name = name'
    _hungerLevel = hungerLevel'

  fun ref setHunger(hungerLevel': U8 = 100) =>
    _hungerLevel = hungerLevel'

  fun getHunger(): U8 => _hungerLevel

actor Aardvark
  let name: String
  var _hungerLevel: U8 = 1

  new create(name': String) =>
    name = name'

  be eat(amount: U8 = 1) =>
    _hungerLevel = _hungerLevel - amount.min(_hungerLevel)

  fun getHunger(): U8 => _hungerLevel
