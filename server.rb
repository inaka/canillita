$: << File.dirname(__FILE__)

require 'pubsub'
require 'goliath'
require 'em-synchrony/activerecord'

class News < ActiveRecord::Base
  self.table_name = 'canillita_news'
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
      when 'POST'
        Pubsub.channel.push(
        {
          :title => env.params["title"] ||= "Generic News", 
          :content => env.params["content"] ||= ""
          })

        [ 204, { }, [ ] ]

      when 'GET'
        sub_id = Pubsub.channel.subscribe do |newsFlash|
          env.stream_send(
            ["event:#{newsFlash[:title]}",
             "data:#{newsFlash[:content]}\n\n"].join("\n"))
        end
        
        env['pubsub.subscriber.id'] = sub_id
        
        Pubsub.callback(:on_close, sub_id) do |e|
          if e['pubsub.subscriber.id'] == sub_id
            Pubsub.channel.unsubscribe(sub_id)
            Pubsub.remove(:on_close, sub_id)
          end
        end
        
        news = News.all
        env.logger.warn news.length
        news.each { |nf|
          env.stream_send(
            ["event:#{nf[:title]}",
             "data:#{nf[:content]}\n\n"].join("\n"))
        }
        streaming_response(200, { 'Content-Type' => "text/event-stream" })

      end
    else raise Goliath::Validation::NotFoundError
    end
  end
end
