package com.nommit.pickle

object SampleDocs {
  val pancakes = """
    @units[cup # @type[volume] @*[cup]]
    @units[tsp # @type[volume] @*[tsp]]

    @[ingredients]
      @ingredient[@volume[1 # @^[cup]] Whole wheat flour # @*[flour]]
      @ingredient[@volume[1 1/2 # @^[tsp]] Baking Powder # @*[baking powder]]
      @ingredient[@volume[1/2 # @^[tsp]] Salt # @*[salt]]
      @ingredient[@quantity[2] Eggs # @*[egg]]
      @ingredient[@volume[1 # @^[cup]] Milk # @*[milk]]
      @ingredient[@volume[1/4 # @^[cup]] Oil # @*[oil]]
    @/
    @[steps]
      Mix together @^[flour], @^[baking powder] and @^[salt] in a bowl. @@
      Separate the @^[egg], reserving the @part[whites # @^[egg]].      @@
      Combine @part[yolks # @^[egg]] with the @^[milk] and @^[oil].
    @/
  """

  val employees = """ +
    @employees[
      @name[John Doe]
      @address[221 B Baker Street]
      |
      @name[Mary Jane]
      @address[221 C Baker Street]
      #
      @collectionType[Set] @elementType[Employee]
    ]
  """

  val foos = """
    @foo[
      @a[] @z[] | @b[] @z[] | @c[]@z[]
    ]
  """
}