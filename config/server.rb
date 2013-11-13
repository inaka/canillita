require 'mysql2'

ActiveRecord::Base.establish_connection(:adapter  => 'em_mysql2',
                                        :database => 'canillita',
                                        :username => 'canillita',
                                        :password => 'extraa!',
                                        :host     => 'localhost',
                                        :pool     => 5)
