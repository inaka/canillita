$: << File.dirname(__FILE__)

require 'pubsub'
require 'goliath'
require 'em-synchrony/activerecord'

class News < ActiveRecord::Base
  self.table_name = 'canillita_news'

  def stream_to(env)
    env.stream_send(
      ["event: #{self.title}",
       "data: #{self.content}\n\n"].join("\n"))
  end
end

class Server < Goliath::API
  use Goliath::Rack::Params

  def on_close(env)
    Pubsub.callbacks(:on_close) { |callback| callback.call(env) }
  end

  def response(env)
    case env['PATH_INFO']
    when "/news"

      case env[Goliath::Request::REQUEST_METHOD]
      when 'DELETE'
        News.delete_all

        # Pubsub.channel.push :close

        [ 204, { }, [ ] ]

      when 'POST'
        newsFlash = News.new(
        {
          :title => env.params["title"] ||= "Generic News", 
          :content => env.params["content"] ||= ""
          })
        newsFlash.save
        Pubsub.channel.push newsFlash

        [ 204, { }, [ ] ]

      when 'GET'
        sub_id = Pubsub.channel.subscribe do |newsFlash|
          newsFlash.stream_to env
        end
        
        env['pubsub.subscriber.id'] = sub_id
        
        Pubsub.callback(:on_close, sub_id) do |e|
          if e['pubsub.subscriber.id'] == sub_id
            Pubsub.channel.unsubscribe(sub_id)
            Pubsub.remove(:on_close, sub_id)
          end
        end
        
        news = News.all
        EM.add_timer(0 ) do
          news.each { |newsFlash|
            newsFlash.stream_to env
          }
        end

        streaming_response(200, { 'Content-Type' => "text/event-stream" })

      end
    else raise Goliath::Validation::NotFoundError
    end
  end
end
