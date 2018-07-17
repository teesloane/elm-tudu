package main

import (
	"fmt"
	"github.com/gin-gonic/gin"
	"github.com/jinzhu/gorm"
	_ "github.com/jinzhu/gorm/dialects/sqlite"
	// "github.com/teesloane/tudu/api/models"
	// "reflect"
)

// Globals

var db *gorm.DB
var err error

// -------------------------------------------------------

func main() {
	initDB()           // Boot db, create if it doesn't exist.
	r := setupRouter() // Starst router.
	r.Run(":8001")
}

func setupRouter() *gin.Engine {
	// gin.DisableConsoleColor() // -- disables colour
	r := gin.Default()
	r.GET("/api/todos", getAllTodos)
	r.GET("/api/todos/:id", getTodo)
	return r
}

func initDB() {

	db, err = gorm.Open("sqlite3", "data.db")
	if err != nil {
		panic("failed to connect to database")
	}
	// defer db.Close() // should figure out when / how / if to close the db.
	db.AutoMigrate(&Todo{})
	db.Create(&Todo{Name: "Test Todo", Complete: true, ParentList: "demoParentlist"})
	fmt.Println("DB: Migrated Todo Table")

}

// --- Model/Handlers: Todo.

type Todo struct {
	gorm.Model
	Name       string
	Complete   bool
	ParentList string
}

func getAllTodos(c *gin.Context) {
	var todos []Todo
	if err := db.Find(&todos).Error; err != nil {
		c.AbortWithStatus(404)
		fmt.Println(err)
	} else {
		c.JSON(200, todos)
	}
}

func getTodo(c *gin.Context) {
	id := c.Params.ByName("id")
	var todo Todo
	if err := db.Where("id = ?", id).First(&todo).Error; err != nil {
		c.AbortWithStatus(404)
		fmt.Println(err)
	} else {
		c.JSON(200, todo)
	}
}
