namespace Examples.TodoLists.Application


type TodoListSimple = { entityId: string; title: string }


type TodoListDetails =
    { entityId: string
      title: string
      items: TodoListItemDetails list }

and TodoListItemDetails =
    { itemId: int
      title: string
      isCompleted: bool }


type TodoListGetDetailsById =
    { entityId: string
      streamSequence: string option }

type TodoListGetDetailsByIdResult = { details: TodoListDetails option }
